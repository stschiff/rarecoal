module Logl (LoglOpt(..), runLogl) where

import RarecoalLib.Utils (GeneralOptions(..), HistogramOptions(..), setNrProcessors, loadHistogram, RarecoalException(..))
import RarecoalLib.MaxUtils (computeLogLikelihood)
import RarecoalLib.ModelTemplate (ModelOptions(..), ParamOptions(..), ModelTemplate(..),
    getModelTemplate, makeParameterDict, instantiateModel)

import Control.Exception (throwIO)

data LoglOpt = LoglOpt {
    loGeneralOpts :: GeneralOptions,
    loModelOpts :: ModelOptions,
    loParamOpts :: ParamOptions,
    loHistogramOpts :: HistogramOptions
}

runLogl :: LoglOpt -> IO ()
runLogl opts = do
    setNrProcessors (loGeneralOpts opts)
    modelTemplate <- getModelTemplate (loModelOpts opts)
    modelParams <- makeParameterDict (loParamOpts opts)
    modelSpec <- case instantiateModel (loGeneralOpts opts) modelTemplate modelParams of
        Left err -> throwIO $ RarecoalModelException err
        Right m -> return m
    let modelBranchNames = mtBranchNames modelTemplate
    (hist, siteRed) <- loadHistogram (loHistogramOpts opts) modelBranchNames
    totalLogLikelihood <- case computeLogLikelihood modelSpec hist modelBranchNames siteRed of
        Left err -> throwIO $ RarecoalCompException err
        Right l -> return l
    print totalLogLikelihood

