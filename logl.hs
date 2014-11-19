module Logl (computeLikelihood, writeSpectrumFile) where

import RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..))
import Core (ModelSpec(..), getProb)
import qualified Data.Map.Strict as Map
import System.Log.Logger (errorM)
import System.Exit (exitFailure)

computeLikelihood :: ModelSpec -> RareAlleleHistogram -> Either String Double
computeLikelihood modelSpec histogram = do
    let standardOrder = computeStandardOrder histogram
        nVec = raNVec histogram
    patternProbs <- mapM (getProb modelSpec nVec) standardOrder
    let patternCounts = map (\o -> defaultLookup $ Pattern o) standardOrder
        ll = sum $ zipWith (\p c -> log p * fromIntegral c) patternProbs patternCounts
        zeroPattern = Pattern $ replicate (length nVec) 0
        otherCounts = defaultLookup zeroPattern + defaultLookup Higher
    return $ ll + fromIntegral otherCounts * log (1.0 - sum patternProbs)
  where
    defaultLookup sitePattern = Map.findWithDefault 0 sitePattern (raCounts histogram)

writeSpectrumFile :: FilePath -> ModelSpec -> RareAlleleHistogram -> IO ()
writeSpectrumFile spectrumFile modelSpec histogram = do
    let standardOrder = computeStandardOrder histogram
        nVec = raNVec histogram
    case mapM (getProb modelSpec nVec) standardOrder of
        Left err -> do
            errorM "rarecoal" $ "Error: " ++ err
            exitFailure
        Right vec ->
            writeFile spectrumFile $ unlines $ zipWith (\p val -> show (Pattern p) ++ "\t" ++ show val) standardOrder vec

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

