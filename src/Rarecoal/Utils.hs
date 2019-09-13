{-# LANGUAGE OverloadedStrings #-}
module Rarecoal.Utils (computeAllConfigs, computeAllConfigsCrude, computeStandardOrder,
    turnHistPatternIntoModelPattern, defaultTimes, getTimeSteps,
    setNrProcessors, filterConditionOn, filterExcludePatterns, filterMaxAf,
    filterGlobalMinAf, GeneralOptions(..), HistogramOptions(..), loadHistogram, ModelBranch, HistBranch,
    choose, chooseCont) where

import SequenceFormats.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern, readHistogram)

import Control.Error
import Control.Monad (when, forM, (>=>), forM_)
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import GHC.Conc (getNumProcessors, setNumCapabilities, getNumCapabilities)
import System.IO (hPutStrLn, stderr)
import Turtle (format, d, (%), w)

type ModelBranch = String
type HistBranch = Text

data GeneralOptions = GeneralOptions {
    optUseCore2 :: Bool,
    optTheta :: Double,
    optNrThreads :: Int,
    optNoShortcut :: Bool,
    optRegPenalty :: Double,
    optN0 :: Int,
    optLinGen :: Int,
    optTMax :: Double
}

data HistogramOptions = HistogramOptions {
    optHistPath :: FilePath,
    optMinAf :: Int,
    optMaxAf :: Int,
    optConditionOn :: [Int],
    optExcludePatterns :: [SitePattern],
    optSiteReduction :: Double
}

defaultTimes :: [Double]
defaultTimes = getTimeSteps 20000 400 20.0

getTimeSteps :: Int -> Int -> Double -> [Double]
getTimeSteps n0 lingen tMax =
    let tMin     = 1.0 / (2.0 * fromIntegral n0)
        alpha    = fromIntegral lingen / (2.0 * fromIntegral n0)
        nr_steps = floor $ logBase (1.0 + tMin / alpha) (1.0 + tMax / alpha)
    in  map (getTimeStep alpha nr_steps) [1..nr_steps-1]
  where
    getTimeStep :: Double -> Int -> Int -> Double
    getTimeStep alpha nr_steps i =
        alpha * exp (fromIntegral i / fromIntegral nr_steps * log (1.0 + tMax / alpha)) - alpha

setNrProcessors :: GeneralOptions -> IO ()
setNrProcessors generalOpts = do
    let nrThreads = optNrThreads generalOpts
    nrProc <- getNumProcessors
    if   nrThreads == 0
    then setNumCapabilities nrProc
    else setNumCapabilities nrThreads
    nrThreads' <- getNumCapabilities
    errLn $ format ("running on "%d%" processors") nrThreads'

computeAllConfigsCrude :: Int -> [Int] -> [SitePattern]
computeAllConfigsCrude maxFreq nVec =
   let maxPowerNum = (maxFreq + 1) ^ nrPop
       order = map (digitize (maxFreq + 1) nrPop) [1..maxPowerNum]
   in  filter (\v -> (sum v <= maxFreq) && and (zipWith (<=) v nVec)) order
  where
    nrPop = length nVec
    digitize :: Int -> Int -> Int -> [Int]
    digitize base nrDigit num
        | nrDigit == 1 = [num]
        | otherwise    = let digitBase = base ^ (nrDigit - 1)
                             digit = div num digitBase
                             rest = num - digit * digitBase
                         in  (digit:digitize base (nrDigit - 1) rest)
    
computeAllConfigs :: Int -> [Int] -> [SitePattern]
computeAllConfigs maxAf nVec =
    filter (\v -> and (zipWith (<=) v nVec)) $ concatMap (allPatternsForAF nrPops) [1..maxAf]
  where
    nrPops = length nVec

allPatternsForAF :: Int -> Int -> [SitePattern]
allPatternsForAF nrPops af = map getDiffs $ allSubsets (nrPops + af - 1) (nrPops - 1)
  where
    getDiffs :: [Int] -> [Int]
    getDiffs subset = go [] ([0] ++ subset ++ [nrPops + af])
    go res (x1:x2:xs) = go (res ++ [x2 - x1 - 1]) (x2:xs)
    go res _ = res

turnHistPatternIntoModelPattern :: [HistBranch] -> [ModelBranch] -> SitePattern -> Either Text SitePattern
turnHistPatternIntoModelPattern histBranches modelBranches histPattern =
    forM modelBranches $ \modelBranchName ->
        case (pack modelBranchName) `elemIndex` histBranches of
            Just i -> return (histPattern !! i)
            Nothing -> return 0 -- ghost branch

allSubsets :: Int -> Int -> [[Int]]
allSubsets n k =
    if k == 0
    then return []
    else do
        True <- return $ n > 0
        allSubsets (n - 1) k ++ map (++[n]) (allSubsets (n - 1) (k - 1))


filterConditionOn :: [Int] -> RareAlleleHistogram ->
    Either Text RareAlleleHistogram
filterConditionOn indices hist =
    if null indices then return hist else do
        assertErr (format ("illegal conditionOn indices "%w) indices) $ all (<n) indices
        let newBody = Map.filterWithKey conditionPatternOn (raCounts hist)
        return $ hist {raCounts = newBody, raConditionOn = indices}
  where
    conditionPatternOn pat _ = all (\i -> pat !! i > 0) indices
    n = length (raNames hist)

filterExcludePatterns :: [[Int]] -> RareAlleleHistogram ->
    Either Text RareAlleleHistogram
filterExcludePatterns excludePatterns hist =
    if null excludePatterns then return hist else do
        assertErr (format ("illegal excludePattern(s)"%w) excludePatterns) $ all ((==n) . length) excludePatterns
        let newBody = Map.filterWithKey pruneExcludePatterns (raCounts hist)
        return $ hist {raExcludePatterns = excludePatterns, raCounts = newBody}
  where
    pruneExcludePatterns pat _ = pat `notElem` excludePatterns
    n = length (raNames hist)

filterMaxAf :: Int -> RareAlleleHistogram -> Either Text RareAlleleHistogram
filterMaxAf maxAf' hist = do
    let maxAf = if maxAf' == 0 then raMaxAf hist else maxAf'
    when (maxAf > raMaxAf hist || maxAf < raMinAf hist) $ Left "illegal maxAF"
    if maxAf == raMaxAf hist then return hist else do
        let newBody = Map.filterWithKey (prunePatternFreq maxAf) (raCounts hist)
        return $ hist {raMaxAf = maxAf, raCounts = newBody}
  where
    prunePatternFreq maxM pat _ = sum pat <= maxM

filterGlobalMinAf :: Int -> RareAlleleHistogram ->
    Either Text RareAlleleHistogram
filterGlobalMinAf minAf hist = do
    when (minAf > raMaxAf hist || minAf < raMinAf hist) $ Left "illegal minAf"
    if minAf == raMinAf hist then return hist else do
        let newBody = Map.filterWithKey prunePatternMinTotalFreq (raCounts hist)
        return $ hist {raCounts = newBody, raMinAf = minAf}
  where
    prunePatternMinTotalFreq pat _ = sum pat >= minAf

computeStandardOrder :: RareAlleleHistogram -> [SitePattern]
computeStandardOrder histogram =
    let nVec = raNVec histogram
    in  filter (\p -> sum p >= raMinAf histogram &&
            hasConditioning (raConditionOn histogram) p &&
            p `notElem` raExcludePatterns histogram) $
            computeAllConfigs (raMaxAf histogram) nVec
  where
    hasConditioning indices pat = all (\i -> pat !! i > 0) indices

loadHistogram :: HistogramOptions -> [ModelBranch] -> Script (RareAlleleHistogram, Double)
loadHistogram histOpts modelBranches = do
    let HistogramOptions path minAf maxAf conditionOn excludePatterns siteRed = histOpts
    hist <- readHistogram path
    validateBranchNameCongruency modelBranches (raNames hist)
    h <- tryRight $ (filterMaxAf maxAf >=> filterGlobalMinAf minAf >=>
        filterConditionOn conditionOn >=> filterExcludePatterns excludePatterns)
        hist
    scriptIO . hPutStrLn stderr $ "loaded histogram " ++ path ++ "with the following options:"
    scriptIO . hPutStrLn stderr $ "Branch Names: " ++ show (raNames hist)
    scriptIO . hPutStrLn stderr $ "NVec: " ++ show (raNVec hist)
    scriptIO . hPutStrLn stderr $ "MinAf: "  ++ show (raMinAf hist)
    scriptIO . hPutStrLn stderr $ "MaxAf: " ++ show (raMaxAf hist)
    scriptIO . hPutStrLn stderr $ "ConditionOn: " ++ show (raConditionOn hist)
    scriptIO . hPutStrLn stderr $ "ExcludePatterns: " ++ show (raExcludePatterns hist)
    scriptIO . hPutStrLn stderr $ "TotalNrSites: " ++ show (raTotalNrSites hist)
    return (h, siteRed)
  where
    validateBranchNameCongruency :: [ModelBranch] -> [HistBranch] -> Script ()
    validateBranchNameCongruency modelBranchNames histBranchNames = do
        forM_ histBranchNames $ \histName ->
            when (unpack histName `notElem` modelBranchNames) $
                throwE (format ("histogram branch "%w%
                    " not found in model branches ("%w%")") histName
                    modelBranchNames)
        forM_ modelBranchNames $ \modelName ->
            when (pack modelName `notElem` histBranchNames) $
                scriptIO . errLn $ format ("found unsampled ghost branch: "%w) modelName

choose :: Int -> Int -> Double
choose _ 0 = 1
choose n k = product [fromIntegral (n + 1 - j) / fromIntegral j | j <- [1..k]]

-- see https://en.wikipedia.org/wiki/Binomial_coefficient
chooseCont :: Double -> Int -> Double
chooseCont _ 0 = 1
chooseCont n k = product [(n + 1.0 - fromIntegral j) / fromIntegral j | j <- [1..k]]

