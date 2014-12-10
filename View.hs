module View (runView, ViewOpt(..)) where

import Data.Int (Int64)
import Control.Error.Script (Script, scriptIO)
import RareAlleleHistogram (reduceIndices, combineIndices, setNrCalledSites, filterMaxAf)
import Control.Monad.Trans.Either (hoistEither)
import Control.Monad (liftM, (<=<))

data ViewOpt = ViewOpt {
    viIndices :: [Int],
    viCombineIndices :: [Int],
    viMaxAf :: Int,
    viNrCalledSites :: Int64,
    viHistPath :: FilePath
}

runView :: ViewOpt -> Script ()
runView opts = do
    hist <- scriptIO . liftM read $ readFile (viHistPath opts)
    hist' <- hoistEither $ (transform <=< return) hist
    scriptIO $ print hist'
  where
   transform = return . combineIndices (viCombineIndices opts)
               <=< setNrCalledSites (viNrCalledSites opts)
               <=< filterMaxAf (viMaxAf opts)
               <=< reduceIndices (viIndices opts) 
