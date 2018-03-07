{-# LANGUAGE OverloadedStrings #-}
module FitTable (runFitTable, FitTableOpt(..)) where

import SequenceFormats.RareAlleleHistogram (RareAlleleHistogram(..))
import Rarecoal.ModelTemplate (ModelOptions(..), ParamOptions(..),
    getModelTemplate, makeParameterDict, instantiateModel, ModelTemplate(..))
import Rarecoal.Utils (GeneralOptions(..), HistogramOptions(..), setNrProcessors, loadHistogram)
import Rarecoal.MaxUtils (computeFrequencySpectrum, computeLogLikelihoodFromSpec, 
    writeFullFitTable, writeSummaryFitTable)

import Control.Error (Script, tryRight, scriptIO)

data FitTableOpt = FitTableOpt {
    ftGeneralOpts :: GeneralOptions,
    ftModelOpts :: ModelOptions,
    ftParamOpts :: ParamOptions,
    ftHistogramOpts :: HistogramOptions,
    ftOutPrefix :: FilePath
}

runFitTable :: FitTableOpt -> Script ()
runFitTable opts = do
    let FitTableOpt generalOpts modelOpts paramOpts histOpts outPrefix = opts
    scriptIO $ setNrProcessors generalOpts
    let outFullTable = outPrefix ++ ".frequencyFitTable.txt"
        outSummaryTable = outPrefix ++ ".summaryFitTable.txt"

    modelTemplate <- getModelTemplate modelOpts
    modelParams <- scriptIO $ makeParameterDict paramOpts
    modelSpec <- tryRight $ instantiateModel (ftGeneralOpts opts )
        modelTemplate modelParams
    let modelBranchNames = mtBranchNames modelTemplate
    hist <- loadHistogram histOpts modelBranchNames
    spectrum <- tryRight $ computeFrequencySpectrum modelSpec (optCoreFunc . ftGeneralOpts $ opts) 
        hist modelBranchNames
    let totalLogLikelihood = computeLogLikelihoodFromSpec (raTotalNrSites hist) spectrum
    scriptIO . putStrLn $ "Log Likelihood:" ++ "\t" ++ show totalLogLikelihood
    scriptIO $ do
        writeFullFitTable outFullTable spectrum
        writeSummaryFitTable outSummaryTable spectrum hist

