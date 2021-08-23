{-# LANGUAGE OverloadedStrings #-}
module FitTable (runFitTable, FitTableOpt(..)) where

import           RarecoalLib.MaxUtils                (computeFrequencySpectrum, computeLogLikelihoodFromSpec,
                                                      writeFullFitTable,
                                                      writeSummaryFitTable)
import           RarecoalLib.ModelTemplate           (ModelOptions (..),
                                                      ModelTemplate (..),
                                                      ParamOptions (..),
                                                      getModelTemplate,
                                                      instantiateModel,
                                                      makeParameterDict)
import           RarecoalLib.Utils                   (GeneralOptions (..),
                                                      HistogramOptions (..),
                                                      loadHistogram,
                                                      setNrProcessors,
                                                      tryEither)
import           SequenceFormats.RareAlleleHistogram (RareAlleleHistogram (..))

data FitTableOpt = FitTableOpt
    { ftGeneralOpts   :: GeneralOptions
    , ftModelOpts     :: ModelOptions
    , ftParamOpts     :: ParamOptions
    , ftHistogramOpts :: HistogramOptions
    , ftOutPrefix     :: FilePath
    }

runFitTable :: FitTableOpt -> IO ()
runFitTable opts = do
    let FitTableOpt generalOpts modelOpts paramOpts histOpts outPrefix = opts
    setNrProcessors generalOpts
    let outFullTable = outPrefix ++ ".frequencyFitTable.txt"
        outSummaryTable = outPrefix ++ ".summaryFitTable.txt"

    modelTemplate <- getModelTemplate modelOpts
    modelParams <- makeParameterDict paramOpts
    modelSpec <- tryEither $ instantiateModel (ftGeneralOpts opts) modelTemplate modelParams
    let modelBranchNames = mtBranchNames modelTemplate
    (hist, siteRed) <- loadHistogram histOpts modelBranchNames
    spectrum <- tryEither $ computeFrequencySpectrum modelSpec hist modelBranchNames
    let totalLogLikelihood = computeLogLikelihoodFromSpec (raTotalNrSites hist) siteRed spectrum
    putStrLn $ "Log Likelihood:" ++ "\t" ++ show totalLogLikelihood
    writeFullFitTable outFullTable spectrum
    writeSummaryFitTable outSummaryTable spectrum hist

