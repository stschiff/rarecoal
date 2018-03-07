module Prob (runProb, ProbOpt(..)) where

import Rarecoal.Utils (GeneralOptions(..), setNrProcessors)
import Rarecoal.Core (getProb)
import Rarecoal.ModelTemplate (ModelOptions(..), ParamOptions(..),
    getModelTemplate, makeParameterDict, instantiateModel)

import Control.Error (Script, scriptIO, tryRight)

data ProbOpt = ProbOpt {
    prGeneralOpts :: GeneralOptions,
    prModelOpts :: ModelOptions,
    prParamOpts :: ParamOptions,
    prNvec :: [Int],
    prKvec :: [Int]
}

runProb :: ProbOpt -> Script ()
runProb opts = do
    scriptIO $ setNrProcessors (prGeneralOpts opts)
    modelTemplate <- getModelTemplate (prModelOpts opts)
    modelParams <- scriptIO $ makeParameterDict (prParamOpts opts)
    modelSpec <- tryRight $ instantiateModel (prGeneralOpts opts)
        modelTemplate modelParams
    let coreFunc = optCoreFunc . prGeneralOpts $ opts
    val <- tryRight $ coreFunc modelSpec (prNvec opts) (prKvec opts)
    scriptIO $ print val
