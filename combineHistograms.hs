import RareAlleleHistogram (RareAlleleHistogram(..), addHistograms)
import System.Environment (getArgs)
import System.IO (FilePath(..), readFile)
import Control.Monad (liftM)

main = do
    args <- getArgs
    newHist <- combine args
    putStr $ show newHist

combine :: [FilePath] -> IO RareAlleleHistogram
combine filenames = do
    histograms <- mapM (liftM read . readFile) filenames
    return $ foldl1 addHistograms histograms
