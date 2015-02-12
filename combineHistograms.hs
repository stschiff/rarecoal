import RareAlleleHistogram (RareAlleleHistogram(..), addHistograms, showHistogram, parseHistogram)
import System.Environment (getArgs)
import Control.Monad (liftM, foldM)
import Control.Error.Script (scriptIO, runScript, Script)
import Control.Monad.Trans.Either (hoistEither)

main :: IO ()
main = runScript $ do
    args <- scriptIO getArgs
    newHist <- combine args
    outs <- hoistEither $ showHistogram newHist
    scriptIO $ putStr outs

combine :: [FilePath] -> Script RareAlleleHistogram
combine filenames = do
    s <- scriptIO $ mapM readFile filenames
    histograms <- mapM (hoistEither . parseHistogram) s 
    hoistEither $ foldM addHistograms (head histograms) (tail histograms)
