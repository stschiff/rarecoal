import RareAlleleHistogram (RareAlleleHistogram(..), addHistograms, showHistogram, parseHistogram)
import System.Environment (getArgs)
import Control.Monad (foldM)
import Control.Error (scriptIO, runScript, Script, tryRight)
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = runScript $ do
    args <- scriptIO getArgs
    newHist <- combine args
    outs <- tryRight $ showHistogram newHist
    scriptIO $ B.putStr outs

combine :: [FilePath] -> Script RareAlleleHistogram
combine filenames = do
    s <- scriptIO $ mapM B.readFile filenames
    histograms <- mapM (tryRight . parseHistogram) s 
    tryRight $ foldM addHistograms (head histograms) (tail histograms)
