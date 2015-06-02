module View (runView, ViewOpt(..)) where

import Data.Int (Int64)
import Control.Error (Script, scriptIO, tryRight)
import RareAlleleHistogram (reduceIndices, combineIndices, setNrCalledSites,
                            filterMaxAf, readHistogramFromHandle, showHistogram)
import Control.Monad ((<=<))
import System.IO (stdin, openFile, IOMode(..))
import qualified Data.Text.IO as T

data ViewOpt = ViewOpt {
    viIndices :: [Int],
    viCombineIndices :: [Int],
    viMaxAf :: Int,
    viNrCalledSites :: Int64,
    viGlobalMax :: Bool,
    viHistPath :: FilePath
}

runView :: ViewOpt -> Script ()
runView opts = do
    let path = viHistPath opts
    handle <- if path == "-" then return stdin else scriptIO $ openFile path ReadMode
    hist <- readHistogramFromHandle handle
    hist' <- tryRight $ (transform <=< return) hist
    outs <- tryRight $ showHistogram hist'
    scriptIO $ T.putStr outs
  where
   transform = combineIndices (viCombineIndices opts)
               <=< if viNrCalledSites opts > 0 then setNrCalledSites (viNrCalledSites opts) else return
               <=< filterMaxAf (viGlobalMax opts) (viMaxAf opts)
               <=< reduceIndices (viIndices opts) 
