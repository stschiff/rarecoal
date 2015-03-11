module Logl (computeLikelihood, runLogl, LoglOpt(..)) where

import RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..), loadHistogram)
import Utils (computeAllConfigs)
import Core (getProb, ModelSpec(..), ModelEvent(..))
import Data.Int (Int64)
import ModelTemplate (getModelSpec)
import Control.Error (Script, scriptIO, assertErr)
import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies (rdeepseq, parMap, parListChunk, using)
import Control.Monad.Trans.Either (hoistEither)
import Debug.Trace (trace)
import Control.Monad (when)

data LoglOpt = LoglOpt {
   loSpectrumPath :: FilePath,
   loTheta :: Double,
   loTemplatePath :: FilePath,
   loParams :: [Double],
   loModelEvents :: [ModelEvent],
   loMaxAf :: Int,
   loNrCalledSites :: Int64,
   loIndices :: [Int],
   loHistPath :: FilePath
}

runLogl :: LoglOpt -> Script ()
runLogl opts = do
    modelSpec <- getModelSpec (loTemplatePath opts) (loTheta opts) (loParams opts) (loModelEvents opts)
    hist <- loadHistogram (loIndices opts) 1 (loMaxAf opts) (loNrCalledSites opts) (loHistPath opts)
    val <- hoistEither $ computeLikelihood modelSpec hist
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
    parMapChunk strat f = (`using` (parListChunk 20) strat) . map f


writeSpectrumFile :: FilePath -> ModelSpec -> RareAlleleHistogram -> Script ()
writeSpectrumFile spectrumFile modelSpec histogram = 
    when (spectrumFile /= "/dev/null") $ do
        standardOrder <- hoistEither $ computeStandardOrder histogram
        let nVec = raNVec histogram
        vec <- hoistEither $ sequence $ parMap rdeepseq (getProb modelSpec nVec) standardOrder
        scriptIO $ writeFile spectrumFile $ unlines $ zipWith (\p val -> show (Pattern p) ++ "\t" ++ show val) standardOrder vec

computeStandardOrder :: RareAlleleHistogram -> Either String [[Int]]
computeStandardOrder histogram =
    if not $ raGlobalMax histogram then
        Left "need global maximum for computeStandardOrder"
    else
        let nrPop = length $ raNVec histogram
            nVec = raNVec histogram
        in  Right $ filter (\p -> sum p >= raMinAf histogram) $ computeAllConfigs nrPop (raMaxAf histogram) nVec

