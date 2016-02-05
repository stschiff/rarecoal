import qualified Options.Applicative as OP
import qualified Pipes.Text.IO as PT
import Data.Monoid ((<>))
import Pipes (runEffect, for, lift)
import Pipes.Attoparsec (parsed)
import Control.Error (runScript, throwE, tryRight, assertErr, scriptIO)
import Rarecoal.FreqSumEntry (FreqSumEntry(..), parseFreqSumEntry)
import Data.List.Split (splitPlaces)
import Control.Monad ((>=>))

data MyOpts = MyOpts [Int] Bool

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "This tool merges samples or groups into larger groups, adding up the allele counts")

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.option OP.auto (OP.short 'n' <> OP.long "nVec" <> OP.metavar "nVec" <> OP.help "comma-separated list of numbers that \
                                       \specify how to join sample or groups, surrounded by square brackets. Example: -n [20,20,1] specifies \
                                       \that you want to merge the first twenty samples/groups into one, and sample 21 through 40, and then \
                                       \have the last group separate. See README for instructions.")
                <*> OP.switch (OP.short 'm' <> OP.long "removeMissing" <> OP.help "if one individual/group has missing data (-1) declare the whole group as missing data.")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts nVec missing) = runScript $ do
    let freqSums = parsed parseFreqSumEntry PT.stdin
    res <- runEffect $ for freqSums $ lift . tryRight . groupFreqSum nVec missing >=> lift . scriptIO . putStrLn . show
    case res of
        Left (err, _) -> throwE $ "Parsing error: " ++ show err
        Right () -> return ()

groupFreqSum :: [Int] -> Bool -> FreqSumEntry -> Either String FreqSumEntry
groupFreqSum nVec missing fs = do
    assertErr "number of samples doesn't match nVec" $ sum nVec == length (fsCounts fs)
    let groups = map sum' . splitPlaces nVec $ fsCounts fs
    return fs {fsCounts = groups}
  where
    sum' values =
        if missing then
            if any (<0) values then -1 else sum values
        else
            sum $ filter (>=0) values
