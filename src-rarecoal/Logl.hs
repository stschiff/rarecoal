module Logl (LoglOpt(..), runLogl, computeLogLikelihood,
    computeStandardOrder) where

import Rarecoal.Options (GeneralOptions(..), ModelOptions(..), HistogramOptions(..))
import Rarecoal.Core (getProb, ModelSpec(..))
import Rarecoal.Formats.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern, showSitePattern)
import Rarecoal.Utils (loadHistogram, computeStandardOrder)
import Rarecoal.ModelTemplate (getModelSpec, ModelDesc)

import Control.Error (Script, scriptIO, assertErr, tryRight, err)
import Control.Parallel.Strategies (rdeepseq, parMap)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import GHC.Conc (getNumCapabilities, setNumCapabilities, getNumProcessors)

data LoglOpt = LoglOpt {
    loGeneralOpts :: GeneralOptions,
    loModelOpts :: ModelOptions,
    loParamOpts :: ParamOpts,
    loHistogramOpts :: HistogramOptions
}

runLogl :: LoglOpt -> Script ()
runLogl opts = do
    setNrProcessors opts
    modelTemplate <- getModelTemplate (loModelOpts opts)
    modelParams <- makeParameterDict (loParamOpts opts)
    modelSpec <- tryRight $ instantiateModel (loGeneralOpts opts )
        modelTemplate modelParams
    hist <- loadHistogram (loHistogramOpts opts) modelTemplate
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
