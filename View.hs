module View (runView, ViewOpt(..)) where

import Data.Int (Int64)
import Control.Error (Script, scriptIO, tryRight)
import RareAlleleHistogram (reduceIndices, combineIndices, setNrCalledSites,
                            filterMaxAf, parseHistogram, showHistogram)
import Control.Monad ((<=<))
import System.IO (stdin, openFile, IOMode(..))
import qualified Data.ByteString.Lazy.Char8 as B

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
    s <- scriptIO $ B.hGetContents handle
    hist <- tryRight $ parseHistogram s
    hist' <- tryRight $ (transform <=< return) hist
    outs <- tryRight $ showHistogram hist'
    scriptIO $ B.putStr outs
  where
   transform = combineIndices (viCombineIndices opts)
               <=< if viNrCalledSites opts > 0 then setNrCalledSites (viNrCalledSites opts) else return
               <=< filterMaxAf (viGlobalMax opts) (viMaxAf opts)
               <=< reduceIndices (viIndices opts) 
