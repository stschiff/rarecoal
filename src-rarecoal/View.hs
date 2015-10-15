module View (runView, ViewOpt(..)) where

import Rarecoal.RareAlleleHistogram (setNrCalledSites,
                            filterMaxAf, readHistogramFromHandle, showHistogram)

import Data.Int (Int64)
import Control.Error (Script, scriptIO, tryRight)
import Control.Monad ((<=<))
import System.IO (stdin, openFile, IOMode(..))
import qualified Data.Text.IO as T

data ViewOpt = ViewOpt {
    viMaxAf :: Int,
    viNrCalledSites :: Int64,
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
   transform = if viNrCalledSites opts > 0 then setNrCalledSites (viNrCalledSites opts) else return
               <=< filterMaxAf (viMaxAf opts)
