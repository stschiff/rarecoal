{-# LANGUAGE OverloadedStrings #-}
module FitTable (runFitTable, FitTableOpt(..)) where

import SequenceFormats.RareAlleleHistogram (RareAlleleHistogram(..))
import RarecoalLib.ModelTemplate (ModelOptions(..), ParamOptions(..),
    getModelTemplate, makeParameterDict, instantiateModel, ModelTemplate(..))
import RarecoalLib.Utils (GeneralOptions(..), HistogramOptions(..), setNrProcessors, loadHistogram, RarecoalException(..))
import RarecoalLib.MaxUtils (computeFrequencySpectrum, computeLogLikelihoodFromSpec, 
    writeFullFitTable, writeSummaryFitTable)

import Control.Exception (throwIO)

data FitTableOpt = FitTableOpt {
    ftGeneralOpts :: GeneralOptions,
    ftModelOpts :: ModelOptions,
    ftParamOpts :: ParamOptions,
    ftHistogramOpts :: HistogramOptions,
    ftOutPrefix :: FilePath
}

runFitTable :: FitTableOpt -> IO ()
runFitTable opts = do
    let FitTableOpt generalOpts modelOpts paramOpts histOpts outPrefix = opts
    setNrProcessors generalOpts
    let outFullTable = outPrefix ++ ".frequencyFitTable.txt"
        outSummaryTable = outPrefix ++ ".summaryFitTable.txt"

    modelTemplate <- getModelTemplate modelOpts
    modelParams <- makeParameterDict paramOpts
    modelSpec <- case instantiateModel (ftGeneralOpts opts) modelTemplate modelParams of
        Left err -> throwIO $ RarecoalModelException err
        Right m -> return m
    let modelBranchNames = mtBranchNames modelTemplate
    (hist, siteRed) <- loadHistogram histOpts modelBranchNames
    spectrum <- case computeFrequencySpectrum modelSpec hist modelBranchNames of
        Left err -> throwIO $ RarecoalHistogramException err
        Right s -> return s
    let totalLogLikelihood = computeLogLikelihoodFromSpec (raTotalNrSites hist) siteRed spectrum
    putStrLn $ "Log Likelihood:" ++ "\t" ++ show totalLogLikelihood
    writeFullFitTable outFullTable spectrum
    writeSummaryFitTable outSummaryTable spectrum hist

