import RareAlleleHistogram (RareAlleleHistogram(..), addHistograms, showHistogram, readHistogram)
import System.Environment (getArgs)
import Control.Monad (foldM)
import Control.Error (scriptIO, runScript, Script, tryRight)
import qualified Data.Text.IO as T

main :: IO ()
main = runScript $ do
    args <- scriptIO getArgs
    newHist <- combine args
    outs <- tryRight $ showHistogram newHist
    scriptIO $ T.putStr outs

combine :: [FilePath] -> Script RareAlleleHistogram
combine filenames = do
    histograms <- mapM readHistogram filenames
    tryRight $ foldM addHistograms (head histograms) (tail histograms)
