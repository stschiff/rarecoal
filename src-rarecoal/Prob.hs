module Prob (runProb, ProbOpt(..)) where

import Rarecoal.Core (getProb)
import Rarecoal.ModelTemplate (getModelSpec, ModelDesc)

import Control.Error (Script, scriptIO, tryRight)

data ProbOpt = ProbOpt {
    prTheta :: Double,
    prModelDesc :: ModelDesc,
    prLinGen :: Int,
    prBranchnames :: [String],
    prNvec :: [Int],
    prKvec :: [Int]
}

runProb :: ProbOpt -> Script ()
runProb opts = do
    modelSpec <- getModelSpec (prModelDesc opts) (prBranchnames opts) (prTheta opts) (prLinGen opts)
    val <- tryRight $ getProb modelSpec (prNvec opts) False (prKvec opts)
    scriptIO $ print val     
