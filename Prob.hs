module Prob (runProb, ProbOpt(..)) where

import ModelTemplate (getModelSpec)
import Core (getProb, ModelEvent(..))
import Control.Error (Script, scriptIO)

data ProbOpt = ProbOpt {
    prTheta :: Double,
    prTemplatePath :: FilePath,
    prParams :: [Double],
    prModelEvents :: [ModelEvent],
    prNvec :: [Int],
    prKvec :: [Int]
}

runProb :: ProbOpt -> Script ()
runProb opts = do
    modelSpec <- getModelSpec (prTemplatePath opts) (prTheta opts) (prParams opts) (prModelEvents opts)
    scriptIO $ print $ getProb modelSpec (prNvec opts) (prKvec opts)
    
