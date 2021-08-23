module Prob (runProb, ProbOpt(..)) where

import qualified RarecoalLib.Core          as C1
import           RarecoalLib.ModelTemplate (ModelOptions (..),
                                            ParamOptions (..), getModelTemplate,
                                            instantiateModel, makeParameterDict)
import           RarecoalLib.StateSpace    (makeJointStateSpace)
import           RarecoalLib.Utils         (GeneralOptions (..),
                                            setNrProcessors, tryEither)

data ProbOpt = ProbOpt
    { prGeneralOpts :: GeneralOptions
    , prModelOpts   :: ModelOptions
    , prParamOpts   :: ParamOptions
    , prNvec        :: [Int]
    , prKvec        :: [Int]
    }

runProb :: ProbOpt -> IO ()
runProb opts = do
    setNrProcessors (prGeneralOpts opts)
    modelTemplate <- getModelTemplate (prModelOpts opts)
    modelParams <- makeParameterDict (prParamOpts opts)
    modelSpec <- tryEither $ instantiateModel (prGeneralOpts opts) modelTemplate modelParams
    let nrPops = length . prNvec $ opts
        maxAf = sum . prKvec $ opts
        stateSpace = makeJointStateSpace nrPops maxAf
    val <- tryEither $ C1.getProb modelSpec stateSpace (prNvec opts) (prKvec opts)
    print val
