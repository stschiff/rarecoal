module View (runView, ViewOpt(..)) where

import Data.Int (Int64)
import Control.Error.Script (Script, scriptIO)
import RareAlleleHistogram (reduceIndices, combineIndices, setNrCalledSites,
                            filterMinAf, filterMaxAf, parseHistogram, showHistogram)
import Control.Monad.Trans.Either (hoistEither)
import Control.Monad (liftM, (<=<))

data ViewOpt = ViewOpt {
    viIndices :: [Int],
    viCombineIndices :: [Int],
    viMinAf :: Int,
    viMaxAf :: Int,
    viNrCalledSites :: Int64,
    viHistPath :: FilePath
}

runView :: ViewOpt -> Script ()
runView opts = do
    s <- scriptIO $ readFile (viHistPath opts)
    hist <- hoistEither $ parseHistogram s
    hist' <- hoistEither $ (transform <=< return) hist
    outs <- hoistEither $ showHistogram hist'
    scriptIO $ putStrLn outs
  where
   transform = return . combineIndices (viCombineIndices opts)
               <=< if viNrCalledSites opts > 0 then setNrCalledSites (viNrCalledSites opts) else return
               <=< filterMinAf (viMinAf opts)
               <=< filterMaxAf (viMaxAf opts)
               <=< reduceIndices (viIndices opts) 
