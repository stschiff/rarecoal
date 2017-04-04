module Rarecoal.Utils (computeAllConfigs, loadHistogram, filterConditionOn, filterExcludePatterns,
    filterMaxAf, filterGlobalMinAf, computeStandardOrder) where

import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..), readHistogram)

import Control.Error
import Control.Monad ((>=>), when)
import qualified Data.Map.Strict as Map

computeAllConfigs :: Int -> Int -> [Int] -> [[Int]]
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

loadHistogram :: Int -> Int -> [Int] -> [[Int]] -> FilePath ->
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
        let newBody = Map.mapKeysWith (+) conditionPatternOn (raCounts hist)
        return $ hist {raCounts = newBody, raConditionOn = indices}
  where
    conditionPatternOn (Pattern pat) =
        if all (\i -> pat !! i > 0) indices then Pattern pat else Higher
    conditionPatternOn Higher = Higher

filterExcludePatterns :: [[Int]] -> RareAlleleHistogram ->
    Either String RareAlleleHistogram
filterExcludePatterns excludePatterns hist =
    if null excludePatterns then return hist else do
        let newBody = Map.mapKeysWith (+) pruneExcludePatterns (raCounts hist)
        return $ hist {raExcludePatterns = excludePatterns, raCounts = newBody}
  where
    pruneExcludePatterns (Pattern pat) =
        if pat `elem` excludePatterns then Higher else Pattern pat
    pruneExcludePatterns Higher = Higher

filterMaxAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterMaxAf maxAf' hist = do
    let maxAf = if maxAf' == 0 then raMaxAf hist else maxAf'
    when (maxAf > raMaxAf hist || maxAf < raMinAf hist) $ Left "illegal maxAF"
    if maxAf == raMaxAf hist then return hist else do
        let newBody = Map.mapKeysWith (+) (prunePatternFreq maxAf) (raCounts hist)
        return $ hist {raMaxAf = maxAf, raCounts = newBody}
  where
    prunePatternFreq maxM (Pattern pat) =
        if sum pat > maxM then Higher else Pattern pat
    prunePatternFreq _ Higher = Higher

filterGlobalMinAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterGlobalMinAf minAf hist = do
    when (minAf > raMaxAf hist || minAf < raMinAf hist) $ Left "illegal minAf"
    if minAf == raMinAf hist then return hist else do
        let newBody = Map.mapKeysWith (+) prunePatternMinTotalFreq (raCounts hist)
        return $ hist {raCounts = newBody, raMinAf = minAf}
  where
    prunePatternMinTotalFreq (Pattern pat) = if sum pat < minAf then Higher else Pattern pat
    prunePatternMinTotalFreq Higher = Higher

computeStandardOrder :: RareAlleleHistogram -> Either String [[Int]]
computeStandardOrder histogram =
    let nrPop = length $ raNVec histogram
        nVec = raNVec histogram
    in  Right . filter (\p -> sum p >= raMinAf histogram &&
            hasConditioning (raConditionOn histogram) p &&
            p `notElem` raExcludePatterns histogram) $
            computeAllConfigs nrPop (raMaxAf histogram) nVec
  where
    hasConditioning indices pat = all (\i -> pat !! i > 0) indices
