module View (runView, ViewOpt(..)) where

import Data.Int (Int64)
import Control.Error.Script (Script, scriptIO)
import RareAlleleHistogram (reduceIndices, combineIndices, setNrCalledSites,
                            filterMinAf, filterMaxAf, parseHistogram, showHistogram)
import Control.Monad.Trans.Either (hoistEither)
import Control.Monad (liftM, (<=<))
import System.IO (stdin, openFile, hGetContents, IOMode(..))

data ViewOpt = ViewOpt {
    viIndices :: [Int],
    viCombineIndices :: [Int],
    viMaxAf :: Int,
    viNrCalledSites :: Int64,
    viHistPath :: FilePath
}

runView :: ViewOpt -> Script ()
runView opts = do
    let path = viHistPath opts
    handle <- if path == "-" then return stdin else scriptIO $ openFile path ReadMode
    s <- scriptIO $ hGetContents handle
    hist <- hoistEither $ parseHistogram s
    hist' <- hoistEither $ (transform <=< return) hist
    outs <- hoistEither $ showHistogram hist'
    scriptIO $ putStrLn outs
  where
   transform = return . combineIndices (viCombineIndices opts)
               <=< if viNrCalledSites opts > 0 then setNrCalledSites (viNrCalledSites opts) else return
               <=< filterMaxAf (viMaxAf opts)
               <=< reduceIndices (viIndices opts) 
