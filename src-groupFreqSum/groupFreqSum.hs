import qualified Options.Applicative as OP
import qualified Pipes.Text.IO as PT
import Data.Monoid ((<>))
import Pipes (runEffect, for, lift)
import Pipes.Attoparsec (parsed)
import Control.Error (runScript, throwE, tryRight, assertErr, scriptIO)
import Rarecoal.FreqSumEntry (FreqSumEntry(..), parseFreqSumEntry)
import Data.List.Split (splitPlaces)
import Control.Monad ((>=>))

data MyOpts = MyOpts [Int]

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) mempty

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.option OP.auto (OP.short 'n' <> OP.long "nVec" <> OP.metavar "nVec")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts nVec) = runScript $ do
    let freqSums = parsed parseFreqSumEntry PT.stdin
    res <- runEffect $ for freqSums $ lift . tryRight . groupFreqSum nVec >=> lift . scriptIO . putStrLn . show
    case res of
        Left (err, _) -> throwE $ "Parsing error: " ++ show err
        Right () -> return ()

groupFreqSum :: [Int] -> FreqSumEntry -> Either String FreqSumEntry
groupFreqSum nVec fs = do
    assertErr "number of samples doesn't match nVec" $ sum nVec == length (fsCounts fs)
    let groups = map sum . splitPlaces nVec $ fsCounts fs
    return fs {fsCounts = groups}
