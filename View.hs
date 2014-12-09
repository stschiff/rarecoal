module View (runView, ViewOpt(..)) where

import Data.Int (Int64)
import Control.Error.Script (Script, scriptIO)
import RareAlleleHistogram (loadHistogram, reduceIndices, combineIndices)
import Control.Monad.Trans.Either (hoistEither)

data ViewOpt = ViewOpt {
    viIndices :: [Int],
    viCombineIndices :: [Int],
    viMaxAf :: Int,
    viNrCalledSites :: Int64,
    viHistPath :: FilePath
}

runView :: ViewOpt -> Script ()
runView opts = do
   hist <- loadHistogram (viMaxAf opts) (viNrCalledSites opts) (viHistPath opts)
   hist' <- hoistEither $ reduceIndices (viIndices opts) hist 
   scriptIO $ print (combineIndices (viCombineIndices opts) hist')
