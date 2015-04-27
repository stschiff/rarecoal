module Prob (runProb, ProbOpt(..)) where

import ModelTemplate (getModelSpec, InitialParams(..))
import Core (getProb, ModelEvent(..))
import Control.Error (Script, scriptIO)
import Control.Monad.Trans.Either (hoistEither)

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
    val <- hoistEither $ getProb modelSpec (prNvec opts) (prKvec opts)
    scriptIO $ print val     
