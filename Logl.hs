module Logl (computeLikelihood, runLogl, LoglOpt(..), InitialParams(..)) where

import RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..), loadHistogram)
import Utils (computeAllConfigs)
import Core (getProb, ModelSpec(..), ModelEvent(..))
import Data.Int (Int64)
import ModelTemplate (getModelSpec, InitialParams(..))
import Control.Error (Script, scriptIO, assertErr, tryRight)
import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies (rdeepseq, parMap)
-- import Debug.Trace (trace)
import Control.Monad (when)

data LoglOpt = LoglOpt {
   loSpectrumPath :: FilePath,
   loTheta :: Double,
   loTemplatePath :: FilePath,
   loParams :: InitialParams,
   loModelEvents :: [ModelEvent],
   loLinGen :: Int,
   loMinAf :: Int,
   loMaxAf :: Int,
   loConditionOn :: [Int],
   loNrCalledSites :: Int64,
   loIndices :: [Int],
   loHistPath :: FilePath
}

runLogl :: LoglOpt -> Script ()
runLogl opts = do
    modelSpec <- getModelSpec (loTemplatePath opts) (loTheta opts) (loParams opts) (loModelEvents opts) (loLinGen opts)
    hist <- loadHistogram (loIndices opts) (loMinAf opts) (loMaxAf opts) (loConditionOn opts) (loNrCalledSites opts) (loHistPath opts)
    val <- tryRight $ computeLikelihood modelSpec hist
    scriptIO $ print val
    writeSpectrumFile (loSpectrumPath opts) modelSpec hist

computeLikelihood :: ModelSpec -> RareAlleleHistogram -> Either String Double
computeLikelihood modelSpec histogram = do
    assertErr "minFreq must be greater than 0" $ raMinAf histogram > 0
    assertErr "maxType of histogram must be global" $ raGlobalMax histogram
    standardOrder <- computeStandardOrder histogram
    let nVec = raNVec histogram
    patternProbs <- sequence $ parMap rdeepseq (getProb modelSpec nVec) standardOrder
    -- patternProbs <- sequence $ parMapChunk rdeepseq (getProb modelSpec nVec) standardOrder
    -- trace (show $ zip standardOrder patternProbs) $ return ()
    let patternCounts = map (defaultLookup . Pattern) standardOrder
        ll = sum $ zipWith (\p c -> log p * fromIntegral c) patternProbs patternCounts
        otherCounts = defaultLookup Higher
    return $ ll + fromIntegral otherCounts * log (1.0 - sum patternProbs)
  where
    defaultLookup sitePattern = Map.findWithDefault 0 sitePattern (raCounts histogram)

writeSpectrumFile :: FilePath -> ModelSpec -> RareAlleleHistogram -> Script ()
writeSpectrumFile spectrumFile modelSpec histogram = 
    when (spectrumFile /= "/dev/null") $ do
        standardOrder <- tryRight $ computeStandardOrder histogram
        let nVec = raNVec histogram
        vec <- tryRight $ sequence $ parMap rdeepseq (getProb modelSpec nVec) standardOrder
        scriptIO $ writeFile spectrumFile $ unlines $ zipWith (\p val -> show (Pattern p) ++ "\t" ++ show val) standardOrder vec

computeStandardOrder :: RareAlleleHistogram -> Either String [[Int]]
computeStandardOrder histogram =
    if not $ raGlobalMax histogram then
        Left "need global maximum for computeStandardOrder"
    else
        let nrPop = length $ raNVec histogram
            nVec = raNVec histogram
        in  Right $ filter (\p -> sum p >= raMinAf histogram && hasConditioning (raConditionOn histogram) p) $ computeAllConfigs nrPop (raMaxAf histogram) nVec
  where
    hasConditioning indices pat = all (\i -> pat !! i > 0) indices
