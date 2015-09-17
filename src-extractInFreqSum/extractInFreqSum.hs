import qualified Options.Applicative as OP
import qualified Pipes.Text.IO as PT
import Data.Monoid ((<>))
import Pipes (runEffect, for, lift)
import Pipes.Attoparsec (parsed)
import Control.Error (runScript, throwE, tryRight, assertErr, scriptIO)
import Rarecoal.FreqSumEntry (FreqSumEntry(..), parseFreqSumEntry)
import Control.Monad ((>=>))

data MyOpts = MyOpts Int

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) mempty

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.option OP.auto (OP.short 'i' <> OP.long "position" <> OP.metavar "<POSITION>")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts position) = runScript $ do
    let freqSums = parsed parseFreqSumEntry PT.stdin
    res <- runEffect $ for freqSums $ lift . tryRight . extractInFreqSum position >=> lift . scriptIO . putStrLn . show
    case res of
        Left (err, _) -> throwE $ "Parsing error: " ++ show err
        Right () -> return ()

extractInFreqSum :: Int -> FreqSumEntry -> Either String FreqSumEntry
extractInFreqSum pos fs = do
    assertErr "position outside bounds" $ pos < length (fsCounts fs)
    let newCounts = take pos (fsCounts fs) ++ drop (pos + 1) (fsCounts fs) ++ [fsCounts fs !! pos]
    return fs {fsCounts = newCounts}
