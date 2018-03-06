{-# LANGUAGE OverloadedStrings #-}
module Rarecoal.Utils (computeAllConfigs, computeStandardOrder,
    turnHistPatternIntoModelPattern, defaultTimes, getTimeSteps,
    setNrProcessors, filterConditionOn, filterExcludePatterns, filterMaxAf,
    filterGlobalMinAf, GeneralOptions(..), HistogramOptions(..), loadHistogram, Branch) where

import SequenceFormats.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern, readHistogram)

import Control.Error
import Control.Monad (when, forM, (>=>), forM_)
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Conc (getNumProcessors, setNumCapabilities, getNumCapabilities)
import Turtle (format, d, (%), w)

type Branch = Text

data GeneralOptions = GeneralOptions {
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
    optExcludePatterns :: [SitePattern]
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

computeAllConfigs :: Int -> Int -> [Int] -> [SitePattern]
computeAllConfigs nrPop maxFreq nVec =
   let maxPowerNum = (maxFreq + 1) ^ nrPop
       order = map (digitize (maxFreq + 1) nrPop) [1..maxPowerNum]
   in  filter (\v -> (sum v <= maxFreq) && and (zipWith (<=) v nVec)) order

turnHistPatternIntoModelPattern :: [Branch] -> [Branch] -> SitePattern -> Either Text SitePattern
turnHistPatternIntoModelPattern histBranches modelBranches histPattern =
    forM modelBranches $ \modelBranchName ->
        case modelBranchName `elemIndex` histBranches of
            Just i -> return (histPattern !! i)
            Nothing -> return 0 -- ghost branch

digitize :: Int -> Int -> Int -> [Int]
digitize base nrDigit num
    | nrDigit == 1 = [num]
    | otherwise    = let digitBase = base ^ (nrDigit - 1)
                         digit = div num digitBase
                         rest = num - digit * digitBase
                     in  (digit:digitize base (nrDigit - 1) rest)

filterConditionOn :: [Int] -> RareAlleleHistogram ->
    Either Text RareAlleleHistogram
filterConditionOn indices hist =
    if null indices then return hist else do
        let newBody = Map.filterWithKey conditionPatternOn (raCounts hist)
        return $ hist {raCounts = newBody, raConditionOn = indices}
  where
    conditionPatternOn pat _ = all (\i -> pat !! i > 0) indices

filterExcludePatterns :: [[Int]] -> RareAlleleHistogram ->
    Either Text RareAlleleHistogram
filterExcludePatterns excludePatterns hist =
    if null excludePatterns then return hist else do
        let newBody = Map.filterWithKey pruneExcludePatterns (raCounts hist)
        return $ hist {raExcludePatterns = excludePatterns, raCounts = newBody}
  where
    pruneExcludePatterns pat _ = pat `notElem` excludePatterns

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
    let nrPop = length $ raNVec histogram
        nVec = raNVec histogram
    in  filter (\p -> sum p >= raMinAf histogram &&
            hasConditioning (raConditionOn histogram) p &&
            p `notElem` raExcludePatterns histogram) $
            computeAllConfigs nrPop (raMaxAf histogram) nVec
  where
    hasConditioning indices pat = all (\i -> pat !! i > 0) indices

loadHistogram :: HistogramOptions -> [Branch] -> Script RareAlleleHistogram
loadHistogram histOpts modelBranches = do
    let HistogramOptions path minAf maxAf conditionOn excludePatterns = histOpts
    hist <- readHistogram path
    validateBranchNameCongruency modelBranches (raNames hist)
    tryRight $ (filterMaxAf maxAf >=> filterGlobalMinAf minAf >=>
        filterConditionOn conditionOn >=> filterExcludePatterns excludePatterns)
        hist
  where
    validateBranchNameCongruency modelBranchNames histBranchNames = do
        forM_ histBranchNames $ \histName ->
            when (histName `notElem` modelBranchNames) $
                throwE (format ("histogram branch "%w%
                    " not found in model branches ("%w%")") histName
                    modelBranchNames)
        forM_ modelBranchNames $ \modelName ->
            when (modelName `notElem` histBranchNames) $
                scriptIO . errLn $ format ("found unsampled ghost branch: "%w)
                modelName