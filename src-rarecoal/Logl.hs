module Logl (LoglOpt(..), runLogl) where

import           RarecoalLib.MaxUtils      (computeLogLikelihood)
import           RarecoalLib.ModelTemplate (ModelOptions (..),
                                            ModelTemplate (..),
                                            ParamOptions (..), getModelTemplate,
                                            instantiateModel, makeParameterDict)
import           RarecoalLib.Utils         (GeneralOptions (..),
                                            HistogramOptions (..),
                                            loadHistogram, setNrProcessors,
                                            tryEither)

data LoglOpt = LoglOpt
    { loGeneralOpts   :: GeneralOptions
    , loModelOpts     :: ModelOptions
    , loParamOpts     :: ParamOptions
    , loHistogramOpts :: HistogramOptions
    }

runLogl :: LoglOpt -> IO ()
runLogl opts = do
    setNrProcessors (loGeneralOpts opts)
    modelTemplate <- getModelTemplate (loModelOpts opts)
    modelParams <- makeParameterDict (loParamOpts opts)
    modelSpec <- tryEither $ instantiateModel (loGeneralOpts opts) modelTemplate modelParams
    let modelBranchNames = mtBranchNames modelTemplate
    (hist, siteRed) <- loadHistogram (loHistogramOpts opts) modelBranchNames
    totalLogLikelihood <- tryEither $ computeLogLikelihood modelSpec hist modelBranchNames siteRed
    print totalLogLikelihood

