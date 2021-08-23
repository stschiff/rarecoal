module Prob (runProb, ProbOpt(..)) where

import Control.Exception (throwIO)

import qualified RarecoalLib.Core as C1
import RarecoalLib.StateSpace (makeJointStateSpace)
import RarecoalLib.Utils (GeneralOptions(..), setNrProcessors, RarecoalException(..))
import RarecoalLib.ModelTemplate (ModelOptions(..), ParamOptions(..),
    getModelTemplate, makeParameterDict, instantiateModel)

data ProbOpt = ProbOpt {
    prGeneralOpts :: GeneralOptions,
    prModelOpts :: ModelOptions,
    prParamOpts :: ParamOptions,
    prNvec :: [Int],
    prKvec :: [Int]
}

runProb :: ProbOpt -> IO ()
runProb opts = do
    setNrProcessors (prGeneralOpts opts)
    modelTemplate <- getModelTemplate (prModelOpts opts)
    modelParams <- makeParameterDict (prParamOpts opts)
    modelSpec <- case instantiateModel (prGeneralOpts opts) modelTemplate modelParams of
        Left err -> throwIO $ RarecoalModelException err
        Right m -> return m
    let nrPops = length . prNvec $ opts
        maxAf = sum . prKvec $ opts
        stateSpace = makeJointStateSpace nrPops maxAf
    val <- case C1.getProb modelSpec stateSpace (prNvec opts) (prKvec opts) of
        Left err -> throwIO $ RarecoalCompException err
        Right v -> return v
    print val
