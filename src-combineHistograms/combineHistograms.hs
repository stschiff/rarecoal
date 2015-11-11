import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), addHistograms, showHistogram, readHistogram)
import System.Environment (getArgs)
import Control.Monad (foldM)
import Control.Error (scriptIO, runScript, Script, tryRight)
import qualified Data.Text.IO as T
import qualified Options.Applicative as OP
import Data.Monoid ((<>))

main :: IO ()
main = OP.execParser parser >>= runWithOptions
  where
    parser = OP.info (OP.helper <*> OP.some parseFileName) (OP.fullDesc <> OP.progDesc "Tool to combine multiple histogram files, for help add option -h")
    parseFileName =
        OP.strArgument $ OP.metavar "histogram_file" <> OP.help "histogram file, put as many as you want to add up"
    
runWithOptions :: [FilePath] -> IO ()
runWithOptions fileNames = runScript $ do
    newHist <- combine fileNames
    outs <- tryRight $ showHistogram newHist
    scriptIO $ T.putStr outs

combine :: [FilePath] -> Script RareAlleleHistogram
combine filenames = do
    histograms <- mapM readHistogram filenames
    tryRight $ foldM addHistograms (head histograms) (tail histograms)
