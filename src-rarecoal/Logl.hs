module Logl (LoglOpt(..), runLogl, computeLogLikelihood,
    computeStandardOrder) where

import Rarecoal.Core (getProb, ModelSpec(..))
import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern, showSitePattern)
import Rarecoal.Utils (loadHistogram, computeStandardOrder)
import Rarecoal.ModelTemplate (getModelSpec, ModelDesc)

import Control.Error (Script, scriptIO, assertErr, tryRight, err)
import Control.Parallel.Strategies (rdeepseq, parMap)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import GHC.Conc (getNumCapabilities, setNumCapabilities, getNumProcessors)

data LoglOpt = LoglOpt {
   loModelDesc :: ModelDesc,
   loMinAf :: Int,
   loMaxAf :: Int,
   loConditionOn :: [Int],
   loExcludePatterns :: [SitePattern],
   loHistPath :: FilePath,
   loNrThreads :: Int
}

runLogl :: LoglOpt -> Script ()
runLogl opts = do
    nrProc <- scriptIO getNumProcessors
    if loNrThreads opts == 0
    then scriptIO $ setNumCapabilities nrProc
    else scriptIO $ setNumCapabilities (loNrThreads opts)
    nrThreads <- scriptIO getNumCapabilities
    scriptIO $ err ("running on " ++ show nrThreads ++ " processors\n")
    hist <- loadHistogram (loMinAf opts) (loMaxAf opts) (loConditionOn opts)
        (loExcludePatterns opts) (loHistPath opts)
    modelSpec <- getModelSpec (loModelDesc opts) (raNames hist)
    standardOrder <- tryRight $ computeStandardOrder hist
    scriptIO . putStrLn $
        "computing probabilities for " ++ show (length standardOrder) ++
        " patterns"
    let nVec = raNVec hist
    patternProbs <- tryRight . sequence $
            parMap rdeepseq (getProb modelSpec nVec False) standardOrder
    let patternCounts = map (defaultLookup hist) standardOrder
        ll = sum $ zipWith (\p c -> log p * fromIntegral c) patternProbs
            patternCounts
        otherCounts = raTotalNrSites hist - sum patternCounts
    let totalLogLikelihood =
            ll + fromIntegral otherCounts * log (1.0 - sum patternProbs)
    scriptIO . putStrLn $ "Log Likelihood:" ++ "\t" ++ show totalLogLikelihood
    scriptIO . mapM_ putStrLn $
        zipWith (\p val -> showSitePattern p ++ "\t" ++ show val) standardOrder
        patternProbs

computeLogLikelihood :: ModelSpec -> RareAlleleHistogram -> Bool ->
    Either String Double
computeLogLikelihood modelSpec histogram noShortcut = do
    assertErr "minFreq must be greater than 0" $ raMinAf histogram > 0
    standardOrder <- computeStandardOrder histogram
    let nVec = raNVec histogram
    patternProbs <- sequence $
        parMap rdeepseq (getProb modelSpec nVec noShortcut) standardOrder
    let patternCounts = map (defaultLookup histogram) standardOrder
        ll = sum $
            zipWith (\p c -> log p * fromIntegral c) patternProbs patternCounts
        otherCounts = raTotalNrSites histogram - sum patternCounts
    return $ ll + fromIntegral otherCounts * log (1.0 - sum patternProbs)

defaultLookup :: RareAlleleHistogram -> SitePattern -> Int64
defaultLookup histogram sitePattern = Map.findWithDefault 0 sitePattern (raCounts histogram)
