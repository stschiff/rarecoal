import Control.Error (runScript, tryAssert, scriptIO, Script, throwE)
import Control.Monad ((>=>))
import Data.Monoid ((<>))
import qualified Options.Applicative as OP
import qualified Pipes.Text.IO as PT
import Pipes (runEffect, for, lift)
import Pipes.Attoparsec (parsed)
import Rarecoal.FreqSumEntry (FreqSumEntry(..), parseFreqSumEntry)
import System.Random (randomIO)

data MyOpts = MyOpts Int Int Int

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "Tool for downsampling a freqSum file.")

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.argument OP.auto (OP.metavar "<POSITION>" <> OP.help "the 0-based index of the population to sample from")
                <*> OP.argument OP.auto (OP.metavar "<N_BEFORE>" <> OP.help "the number of haplotypes in the group before sampling")
                <*> OP.argument OP.auto (OP.metavar "<N_AFTER>" <> OP.help "the new number of haplotypes to downsample to")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts position nBefore nAfter) = runScript $ do
    tryAssert "nBefore has to be >= nAfter" $ nBefore >= nAfter
    let freqSums = parsed parseFreqSumEntry PT.stdin
    res <- runEffect $ for freqSums $ lift . downSample position nBefore nAfter >=> lift . scriptIO . putStrLn . show
    case res of
        Left (err, _) -> throwE $ "Parsing error: " ++ show err
        Right () -> return ()

downSample :: Int -> Int -> Int -> FreqSumEntry -> Script FreqSumEntry
downSample pos nBefore nAfter fs = do
    tryAssert "position outside bounds" $ pos < length (fsCounts fs)
    newK <- scriptIO $ sampleWithoutReplacement nBefore (fsCounts fs !! pos) nAfter
    let newCounts = take pos (fsCounts fs) ++ [newK] ++ drop (pos + 1) (fsCounts fs)
    return fs {fsCounts = newCounts}

sampleWithoutReplacement :: Int -> Int -> Int -> IO Int
sampleWithoutReplacement n k howMany = go n k howMany 0
  where
    go _ _ 0 ret = return ret
    go _ 0 _ ret = return ret
    go n' k' howMany' ret = do
        val <- bernoulli $ fromIntegral k' / fromIntegral n'
        if val then go (n' - 1) (k' - 1) (howMany' - 1) (ret + 1) else go (n' - 1) k' (howMany' - 1) ret

bernoulli :: Double -> IO Bool
bernoulli p = (<p) <$> randomIO
