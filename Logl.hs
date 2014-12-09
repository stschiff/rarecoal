module Logl (computeLikelihood, runLogl, LoglOpt(..)) where

import RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..), loadHistogram)
import Core (getProb, ModelSpec(..), ModelEvent(..))
import Data.Int (Int64)
import ModelTemplate (getModelSpec)
import Control.Error (Script, scriptIO)
import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies (rdeepseq, parMap)

data LoglOpt = LoglOpt {
   loSpectrumPath :: FilePath,
   loTheta :: Double,
   loTemplatePath :: FilePath,
   loParams :: [Double],
   loModelEvents :: [ModelEvent],
   loIndices :: [Int],
   loMaxAf :: Int,
   loNrCalledSites :: Int64,
   loHistPath :: FilePath
}

runLogl :: LoglOpt -> Script ()
runLogl opts = do
    modelSpec <- getModelSpec (loTemplatePath opts) (loTheta opts) (loParams opts) (loModelEvents opts)
    hist <- loadHistogram (loIndices opts) (loMaxAf opts) (loNrCalledSites opts) (loHistPath opts)
    scriptIO $ print $ computeLikelihood modelSpec hist 
    scriptIO $ writeSpectrumFile (loSpectrumPath opts) modelSpec hist

computeLikelihood :: ModelSpec -> RareAlleleHistogram -> Double
computeLikelihood modelSpec histogram =
    let standardOrder = computeStandardOrder histogram
        nVec = raNVec histogram
        patternProbs = parMap rdeepseq (getProb modelSpec nVec) standardOrder
        patternCounts = map (defaultLookup . Pattern) standardOrder
        ll = sum $ zipWith (\p c -> log p * fromIntegral c) patternProbs patternCounts
        zeroPattern = Pattern $ replicate (length nVec) 0
        otherCounts = defaultLookup zeroPattern + defaultLookup Higher
    in  ll + fromIntegral otherCounts * log (1.0 - sum patternProbs)
  where
    defaultLookup sitePattern = Map.findWithDefault 0 sitePattern (raCounts histogram)

writeSpectrumFile :: FilePath -> ModelSpec -> RareAlleleHistogram -> IO ()
writeSpectrumFile spectrumFile modelSpec histogram =
    let standardOrder = computeStandardOrder histogram
        nVec = raNVec histogram
        vec = parMap rdeepseq (getProb modelSpec nVec) standardOrder
    in  writeFile spectrumFile $ unlines $ zipWith (\p val -> show (Pattern p) ++ "\t" ++ show val) standardOrder vec

computeStandardOrder :: RareAlleleHistogram -> [[Int]]
computeStandardOrder histogram =
    let nrPop = length $ raNVec histogram
        maxPowerNum = (raMaxAf histogram + 1) ^ nrPop
        order = map (digitize (raMaxAf histogram + 1) nrPop) [1..maxPowerNum]
    in  if raGlobalMax histogram
            then filter (\v -> sum v <= raMaxAf histogram) order
            else order
    
digitize :: Int -> Int -> Int -> [Int]
digitize base nrDigit num 
    | nrDigit == 1 = [num]
    | otherwise    = let digitBase = base ^ (nrDigit - 1)
                         digit = div num digitBase
                         rest = num - digit * digitBase
                     in  (digit:digitize base (nrDigit - 1) rest)  

