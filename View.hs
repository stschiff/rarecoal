module View (runView, ViewOpt(..)) where

import Data.Int (Int64)
import Control.Error.Script (Script, scriptIO)
import RareAlleleHistogram (loadHistogram)

data ViewOpt = ViewOpt {
    viIndices :: [Int],
    viMaxAf :: Int,
    viNrCalledSites :: Int64,
    viHistPath :: FilePath
}

runView :: ViewOpt -> Script ()
runView opts = do
   hist <- loadHistogram (viIndices opts) (viMaxAf opts) (viNrCalledSites opts) (viHistPath opts)
   scriptIO $ print hist 
