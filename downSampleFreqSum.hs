import qualified Options.Applicative as OP
import qualified Pipes.Text.IO as PT
import Pipes (runEffect, for, lift)
import Pipes.Attoparsec (parsed)
import Control.Error (runScript, left, tryAssert, scriptIO, Script)
import FreqSumEntry (FreqSumEntry(..), parseFreqSumEntry)
import Control.Monad ((>=>))
import System.Random (randomIO)

data MyOpts = MyOpts Int Int Int

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) mempty

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.argument OP.auto (OP.metavar "<POSITION>")
                <*> OP.argument OP.auto (OP.metavar "<N_BEFORE>")
                <*> OP.argument OP.auto (OP.metavar "<N_AFTER>")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts position nBefore nAfter) = runScript $ do
    tryAssert "nBefore has to be >= nAfter" $ nBefore >= nAfter
    let freqSums = parsed parseFreqSumEntry PT.stdin
    res <- runEffect $ for freqSums $ lift . downSample position nBefore nAfter >=> lift . scriptIO . putStrLn . show
    case res of
        Left (err, _) -> left $ "Parsing error: " ++ show err
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
    go n k howMany ret = do
        val <- bernoulli $ fromIntegral k / fromIntegral n
        if val then go (n - 1) (k - 1) (howMany - 1) (ret + 1) else go (n - 1) k (howMany - 1) ret
    
bernoulli :: Double -> IO Bool
bernoulli p = (<p) <$> randomIO