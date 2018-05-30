module Prob (runProb, ProbOpt(..)) where

import qualified Rarecoal.Core as C1
import qualified Rarecoal.Core2 as C2
import Rarecoal.StateSpace (makeJointStateSpace)
import Rarecoal.Utils (GeneralOptions(..), setNrProcessors)
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
    let nrPops = length . prNvec $ opts
        maxAf = sum . prKvec $ opts
        stateSpace = makeJointStateSpace nrPops maxAf
        coreFunc = if optUseCore2 . prGeneralOpts $ opts
            then C2.getProb
            else C1.getProb
    val <- tryRight $ coreFunc modelSpec stateSpace (prNvec opts) (prKvec opts)
    scriptIO $ print val
