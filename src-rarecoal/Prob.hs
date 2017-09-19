module Prob (runProb, ProbOpt(..)) where

import Rarecoal.Options (GeneralOptions(..), ModelOptions(..), HistogramOptions(..))
import Rarecoal.Core (getProb)
import Rarecoal.ModelTemplate (getModelSpec, ModelDesc)

import Control.Error (Script, scriptIO, tryRight)

data ProbOpt = ProbOpt {
    prModelOpts :: ModelOptions,
    prParamOpts :: ParamOptions,
    prNvec :: [Int],
    prKvec :: [Int]
}

runProb :: ProbOpt -> Script ()
runProb opts = do
    setNrProcessors opts
    modelTemplate <- getModelTemplate (prModelOpts opts)
    modelParams <- makeParameterDict (prParamOpts opts)
    modelSpec <- tryRight $ instantiateModel (prGeneralOpts opts)
        modelTemplate modelParams

    val <- tryRight $ getProb modelSpec (prNvec opts) False (prKvec opts)
    scriptIO $ print val
