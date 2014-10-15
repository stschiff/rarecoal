import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty)
import System.IO (openFile, IOMode(..), FilePath, hGetLine, hClose)
import qualified Options.Applicative as OP
import OrderedZip (orderedZip)
import FreqSumEntry (FreqSumEntry(..))
import qualified Pipes.Prelude as P
import Pipes ((>->), runEffect)
import Control.Exception (assert)
import Control.Monad (liftM)

data MyOpts = MyOpts FilePath FilePath

main = OP.execParser opts >>= runWithOptions
  where
    parser = MyOpts <$> OP.argument OP.str (OP.metavar "freqSumFile1")
                    <*> OP.argument OP.str (OP.metavar "freqSumFile2")
    opts = OP.info parser mempty

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts f1 f2) = do
    n1 <- readNfromFile f1
    n2 <- readNfromFile f2
    h1 <- openFile f1 ReadMode
    h2 <- openFile f2 ReadMode
    let p1 = P.fromHandle h1 >-> P.map read
        p2 = P.fromHandle h2 >-> P.map read
    runEffect $ orderedZip compare p1 p2 >-> P.map (freqSumCombine n1 n2) >-> P.map show >-> P.stdoutLn
    hClose h1
    hClose h2

readNfromFile :: FilePath -> IO Int
readNfromFile fn = do
    h <- openFile fn ReadMode
    fs <- liftM read $ hGetLine h
    hClose h
    return $ length (fsCounts fs)
    
freqSumCombine :: Int -> Int -> (Maybe FreqSumEntry, Maybe FreqSumEntry) -> FreqSumEntry
freqSumCombine n1 n2 (Just fs1, Nothing) = fs1 {fsCounts = fsCounts fs1 ++ replicate n2 0}
freqSumCombine n1 n2 (Nothing, Just fs2) = fs2 {fsCounts = replicate n1 0 ++ fsCounts fs2}
freqSumCombine n1 n2 (Just fs1, Just fs2) =
    if fsChrom fs1 == fsChrom fs2 && fsRef fs1 == fsRef fs2 && fsAlt fs1 == fsAlt fs2
        then fs1 {fsCounts = fsCounts fs1 ++ fsCounts fs2}
        else fs1 {fsCounts = fsCounts fs1 ++ replicate n2 0}
