import RareAlleleHistogram (RareAlleleHistogram(..), addHistograms)
import System.Environment (getArgs)
import Control.Monad (liftM, foldM)
import Control.Error.Script (scriptIO, runScript, Script)
import Control.Monad.Trans.Either (hoistEither)

main :: IO ()
main = runScript $ do
    args <- scriptIO getArgs
    newHist <- combine args
    scriptIO $ putStr (show newHist)

combine :: [FilePath] -> Script RareAlleleHistogram
combine filenames = do
    histograms <- scriptIO $ mapM (liftM read . readFile) filenames
    hoistEither $ foldM addHistograms (head histograms) (tail histograms)
