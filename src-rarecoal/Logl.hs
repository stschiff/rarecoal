module Logl (LoglOpt(..), runLogl, SpectrumEntry(..), Spectrum,
    computeFrequencySpectrum, computeLogLikelihood) where

import Rarecoal.Core (getProb)
import Rarecoal.Formats.RareAlleleHistogram (RareAlleleHistogram(..),
    showSitePattern)
import Rarecoal.Utils (computeStandardOrder, GeneralOptions(..),
    HistogramOptions(..), setNrProcessors, turnHistPatternIntoModelPattern)
import Rarecoal.ModelTemplate (ModelOptions(..), ParamOptions(..),
    loadHistogram, getModelTemplate, makeParameterDict, instantiateModel, getNrAndNamesOfBranches)

import Control.Error (Script, scriptIO, tryRight)
import Control.Parallel.Strategies (rdeepseq, parMap)
import qualified Data.Map.Strict as Map

data LoglOpt = LoglOpt {
    loGeneralOpts :: GeneralOptions,
    loModelOpts :: ModelOptions,
    loParamOpts :: ParamOptions,
    loHistogramOpts :: HistogramOptions
}

data SpectrumEntry = SpectrumEntry {
    spPattern :: SitePattern,
    spCount :: Int64,
    spFreq :: Double,
    spJackknifeError :: Maybe Double,
    spTheory :: Double
}

type Spectrum = [SpectrumEntry]

runLogl :: LoglOpt -> Script ()
runLogl opts = do
    scriptIO $ setNrProcessors (loGeneralOpts opts)
    modelTemplate <- getModelTemplate (loModelOpts opts)
    modelParams <- scriptIO $ makeParameterDict (loParamOpts opts)
    modelSpec <- tryRight $ instantiateModel (loGeneralOpts opts )
        modelTemplate modelParams
    hist <- loadHistogram (loHistogramOpts opts) modelTemplate
    (_, modelBranchNames) <- tryRight $ getNrAndNamesOfBranches modelTemplate
    spectrum <- computeFrequencySpectrum modelSpec hist modelBranchNames
    let totalLogLikelihood = computeLogLikelihood spectrum
    scriptIO . putStrLn $ "Log Likelihood:" ++ "\t" ++ show totalLogLikelihood
    scriptIO . mapM_ putStrLn $
        zipWith (\(p, _, val) -> showSitePattern p ++ "\t" ++ show val) spectrum

computeFrequencySpectrum :: ModelSpec -> RareAlleleHistogram -> [String] ->
    Either Text Spectrum
computeFrequencySpectrum modelSpec histogram modelBranchNames = do
    assertErr "minFreq must be greater than 0" $ raMinAf histogram > 0
    let standardOrder = computeStandardOrder histogram
    scriptIO . putStrLn $
        "computing probabilities for " ++ show (length standardOrder) ++
        " patterns"
    standardOrderModelMapped <-
        mapM (turnHistPatternIntoModelPattern histogram modelBranchNames)
            standardOrder
    let nVec = raNVec histogram
    nVecModelMapped <-
        turnHistPatternIntoModelPattern histogram modelBranchNames nVec
    patternProbs <- sequence $
        parMap rdeepseq (getProb modelSpec nVecModelMapped)
            standardOrderModelMapped
    let patternCounts =
            [Map.findWithDefault 0 k (raCounts histogram) | k <- standardOrder]
        totalCounts = raTotalNrSites histogram
        patternFreqs = [c / fromIntegral totalCounts | c <- patternCounts]
        errors = case raJackknifeEstimates histogram of
            Just errMap ->
                [Just . snd $ Map.findWithDefault (0, 0) k errMap | k <- standardOrder]
            Nothing -> [Nothing | _ <- standardOrder]
    return $ zipWith5 SpectrumEntry standardOrder patternCounts patternFreqs
        patternProbs errors

computeLogLikelihood :: Spectrum -> Double
computeLogLikelihood spectrum = do
    let ll = sum [log (spTheory e) * fromIntegral (spCount e) | e <- spectrum]
        patternCounts = map spCount spectrum
        patternProbs = map spTheory spectrum
        otherCounts = raTotalNrSites histogram - sum patternCounts
    return $ ll + fromIntegral otherCounts * log (1.0 - sum patternProbs)
