module Rarecoal.Utils (computeAllConfigs, loadHistogram, filterConditionOn, filterExcludePatterns,
    filterMaxAf, filterGlobalMinAf, computeStandardOrder) where

import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern, readHistogram)

import Control.Error
import Control.Monad ((>=>), when)
import qualified Data.Map.Strict as Map

computeAllConfigs :: Int -> Int -> [Int] -> [SitePattern]
computeAllConfigs nrPop maxFreq nVec = 
   let maxPowerNum = (maxFreq + 1) ^ nrPop
       order = map (digitize (maxFreq + 1) nrPop) [1..maxPowerNum]
   in  filter (\v -> (sum v <= maxFreq) && and (zipWith (<=) v nVec)) order

digitize :: Int -> Int -> Int -> [Int]
digitize base nrDigit num 
    | nrDigit == 1 = [num]
    | otherwise    = let digitBase = base ^ (nrDigit - 1)
                         digit = div num digitBase
                         rest = num - digit * digitBase
                     in  (digit:digitize base (nrDigit - 1) rest)  

loadHistogram :: Int -> Int -> [Int] -> [SitePattern] -> FilePath ->
    Script RareAlleleHistogram
loadHistogram minAf maxAf conditionOn excludePatterns path = do
    hist <- readHistogram path
    tryRight $ (filterMaxAf maxAf >=> filterGlobalMinAf minAf >=>
        filterConditionOn conditionOn >=> filterExcludePatterns excludePatterns)
        hist

filterConditionOn :: [Int] -> RareAlleleHistogram ->
    Either String RareAlleleHistogram
filterConditionOn indices hist =
    if null indices then return hist else do
        let newBody = Map.filterWithKey conditionPatternOn (raCounts hist)
        return $ hist {raCounts = newBody, raConditionOn = indices}
  where
    conditionPatternOn pat _ = all (\i -> pat !! i > 0) indices

filterExcludePatterns :: [[Int]] -> RareAlleleHistogram ->
    Either String RareAlleleHistogram
filterExcludePatterns excludePatterns hist =
    if null excludePatterns then return hist else do
        let newBody = Map.filterWithKey pruneExcludePatterns (raCounts hist)
        return $ hist {raExcludePatterns = excludePatterns, raCounts = newBody}
  where
    pruneExcludePatterns pat _ = not $ pat `elem` excludePatterns 

filterMaxAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterMaxAf maxAf' hist = do
    let maxAf = if maxAf' == 0 then raMaxAf hist else maxAf'
    when (maxAf > raMaxAf hist || maxAf < raMinAf hist) $ Left "illegal maxAF"
    if maxAf == raMaxAf hist then return hist else do
        let newBody = Map.filterWithKey (prunePatternFreq maxAf) (raCounts hist)
        return $ hist {raMaxAf = maxAf, raCounts = newBody}
  where
    prunePatternFreq maxM pat _ = sum pat <= maxM

filterGlobalMinAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterGlobalMinAf minAf hist = do
    when (minAf > raMaxAf hist || minAf < raMinAf hist) $ Left "illegal minAf"
    if minAf == raMinAf hist then return hist else do
        let newBody = Map.filterWithKey prunePatternMinTotalFreq (raCounts hist)
        return $ hist {raCounts = newBody, raMinAf = minAf}
  where
    prunePatternMinTotalFreq pat _ = sum pat >= minAf

computeStandardOrder :: RareAlleleHistogram -> Either String [SitePattern]
computeStandardOrder histogram =
    let nrPop = length $ raNVec histogram
        nVec = raNVec histogram
    in  Right . filter (\p -> sum p >= raMinAf histogram &&
            hasConditioning (raConditionOn histogram) p &&
            p `notElem` raExcludePatterns histogram) $
            computeAllConfigs nrPop (raMaxAf histogram) nVec
  where
    hasConditioning indices pat = all (\i -> pat !! i > 0) indices
