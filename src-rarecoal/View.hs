module View (runView, ViewOpt(..)) where

import Rarecoal.RareAlleleHistogram (filterMaxAf, readHistogramFromHandle, showHistogram)

import Control.Error (Script, scriptIO, tryRight)
import System.IO (stdin, openFile, IOMode(..))
import qualified Data.Text.IO as T

data ViewOpt = ViewOpt {
    viMaxAf :: Int,
    viHistPath :: FilePath
}

runView :: ViewOpt -> Script ()
runView opts = do
    let path = viHistPath opts
    handle <- if path == "-" then return stdin else scriptIO $ openFile path ReadMode
    hist <- readHistogramFromHandle handle
    hist' <- tryRight $ filterMaxAf (viMaxAf opts) hist
    outs <- tryRight $ showHistogram hist'
    scriptIO $ T.putStr outs
