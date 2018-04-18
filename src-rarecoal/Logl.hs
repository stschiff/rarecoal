module Logl (LoglOpt(..), runLogl) where

import Rarecoal.Utils (GeneralOptions(..), HistogramOptions(..), setNrProcessors, loadHistogram)
import Rarecoal.MaxUtils (computeLogLikelihood)
import Rarecoal.ModelTemplate (ModelOptions(..), ParamOptions(..), ModelTemplate(..),
    getModelTemplate, makeParameterDict, instantiateModel)

import Control.Error (Script, scriptIO, tryRight)

data LoglOpt = LoglOpt {
    loGeneralOpts :: GeneralOptions,
    loModelOpts :: ModelOptions,
    loParamOpts :: ParamOptions,
    loHistogramOpts :: HistogramOptions
}

runLogl :: LoglOpt -> Script ()
runLogl opts = do
    scriptIO $ setNrProcessors (loGeneralOpts opts)
    modelTemplate <- getModelTemplate (loModelOpts opts)
    modelParams <- scriptIO $ makeParameterDict (loParamOpts opts)
    modelSpec <- tryRight $ instantiateModel (loGeneralOpts opts) modelTemplate modelParams
    let modelBranchNames = mtBranchNames modelTemplate
    (hist, siteRed) <- loadHistogram (loHistogramOpts opts) modelBranchNames
    totalLogLikelihood <- tryRight $ computeLogLikelihood modelSpec
        (optCoreFunc . loGeneralOpts $ opts) hist modelBranchNames siteRed
    scriptIO . print $ totalLogLikelihood

