module Logl (LoglOpt(..), runLogl, computeLikelihood) where

import Rarecoal.Core (getProb, ModelSpec(..))
import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..), loadHistogram)
import Rarecoal.Utils (computeAllConfigs)
import Rarecoal.ModelTemplate (getModelSpec, ModelDesc)

import Control.Error (Script, scriptIO, assertErr, tryRight, err)
import Control.Parallel.Strategies (rdeepseq, parMap)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import GHC.Conc (getNumCapabilities, setNumCapabilities, getNumProcessors)

data LoglOpt = LoglOpt {
   loTheta :: Double,
   loModelDesc :: ModelDesc,
   loLinGen :: Int,
   loMinAf :: Int,
   loMaxAf :: Int,
   loConditionOn :: [Int],
   loHistPath :: FilePath,
   loNrThreads :: Int
}

runLogl :: LoglOpt -> Script ()
runLogl opts = do
    nrProc <- scriptIO getNumProcessors
    if (loNrThreads opts == 0)
    then scriptIO $ setNumCapabilities nrProc
    else scriptIO $ setNumCapabilities (loNrThreads opts)
    nrThreads <- scriptIO getNumCapabilities
    scriptIO $ err ("running on " ++ show nrThreads ++ " processors\n")
    modelSpec <- getModelSpec (loModelDesc opts) (loTheta opts) (loLinGen opts)
    hist <- loadHistogram (loMinAf opts) (loMaxAf opts) (loConditionOn opts) (loHistPath opts)
    standardOrder <- tryRight $ computeStandardOrder hist
    let nVec = raNVec hist
    patternProbs <- tryRight . sequence $
            parMap rdeepseq (getProb modelSpec nVec False) standardOrder
    let patternCounts = map (defaultLookup hist . Pattern) standardOrder
        ll = sum $ zipWith (\p c -> log p * fromIntegral c) patternProbs patternCounts
        otherCounts = defaultLookup hist Higher
    let totalLogLikelihood = ll + fromIntegral otherCounts * log (1.0 - sum patternProbs)
    scriptIO . putStrLn $ "Log Likelihood:" ++ "\t" ++ show totalLogLikelihood
    scriptIO . mapM_ putStrLn $
        zipWith (\p val -> show (Pattern p) ++ "\t" ++ show val) standardOrder patternProbs

computeStandardOrder :: RareAlleleHistogram -> Either String [[Int]]
computeStandardOrder histogram =
    let nrPop = length $ raNVec histogram
        nVec = raNVec histogram
    in  Right $ filter (\p -> sum p >= raMinAf histogram && hasConditioning (raConditionOn histogram) p) $ computeAllConfigs nrPop (raMaxAf histogram) nVec
  where
    hasConditioning indices pat = all (\i -> pat !! i > 0) indices

computeLikelihood :: ModelSpec -> RareAlleleHistogram -> Bool -> Either String Double
computeLikelihood modelSpec histogram noShortcut = do
    assertErr "minFreq must be greater than 0" $ raMinAf histogram > 0
    standardOrder <- computeStandardOrder histogram
    let nVec = raNVec histogram
    patternProbs <- sequence $ parMap rdeepseq (getProb modelSpec nVec noShortcut) standardOrder
    let patternCounts = map (defaultLookup histogram . Pattern) standardOrder
        ll = sum $ zipWith (\p c -> log p * fromIntegral c) patternProbs patternCounts
        otherCounts = defaultLookup histogram Higher
    return $ ll + fromIntegral otherCounts * log (1.0 - sum patternProbs)

defaultLookup :: RareAlleleHistogram -> SitePattern -> Int64
defaultLookup histogram sitePattern = Map.findWithDefault 0 sitePattern (raCounts histogram)
