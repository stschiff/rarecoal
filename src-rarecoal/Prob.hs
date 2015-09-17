module Prob (runProb, ProbOpt(..)) where

import ModelTemplate (getModelSpec, InitialParams(..))
import Core (getProb, ModelEvent(..))
import Control.Error (Script, scriptIO, tryRight)

data ProbOpt = ProbOpt {
    prTheta :: Double,
    prTemplatePath :: FilePath,
    prParams :: InitialParams,
    prModelEvents :: [ModelEvent],
    prLinGen :: Int,
    prNvec :: [Int],
    prKvec :: [Int]
}

runProb :: ProbOpt -> Script ()
runProb opts = do
    modelSpec <- getModelSpec (prTemplatePath opts) (prTheta opts) (prParams opts) (prModelEvents opts) (prLinGen opts)
    val <- tryRight $ getProb modelSpec (prNvec opts) False (prKvec opts)
    scriptIO $ print val     
