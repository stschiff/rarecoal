import System.IO (openFile, IOMode(..), hGetLine, hClose)
import qualified Options.Applicative as OP
import OrderedZip (orderedZip)
import qualified Pipes.Prelude as P
import Pipes ((>->), runEffect, for)
import Control.Monad.Trans.Class (lift)
import FreqSumEntry (FreqSumEntry(..), parseFreqSumEntry)
import Pipes.Attoparsec (parsed)
import qualified Pipes.Text.IO as PT
import qualified Data.Text as T
import Control.Error (runScript, Script, scriptIO, left, tryRight)
import Data.Attoparsec.Text (parseOnly)

data MyOpts = MyOpts FilePath FilePath

main = OP.execParser opts >>= runWithOptions
  where
    parser = MyOpts <$> OP.argument OP.str (OP.metavar "freqSumFile1")
                    <*> OP.argument OP.str (OP.metavar "freqSumFile2")
    opts = OP.info parser mempty

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts f1 f2) = runScript $ do
    n1 <- readNfromFile f1
    n2 <- readNfromFile f2
    h1 <- scriptIO $ openFile f1 ReadMode
    h2 <- scriptIO $ openFile f2 ReadMode
    let p1 = parsed parseFreqSumEntry . PT.fromHandle $ h1
        p2 = parsed parseFreqSumEntry . PT.fromHandle $ h2
        combinedProd = orderedZip comp p1 p2 >-> P.map (freqSumCombine n1 n2)
    res <- runEffect $ for combinedProd $ lift . scriptIO . putStrLn . show
    case res of
        Left (err, _) -> left $ "Parsing error: " ++ show err
        Right () -> return ()
    scriptIO . hClose $ h1
    scriptIO . hClose $ h2
  where
    comp fs1 fs2 = fsPos fs1 `compare` fsPos fs2

readNfromFile :: FilePath -> Script Int
readNfromFile fn = do
    h <- scriptIO $ openFile fn ReadMode
    l <- scriptIO . hGetLine $ h
    fs <- tryRight . parseOnly parseFreqSumEntry . flip T.snoc '\n' . T.pack $ l
    scriptIO $ hClose h
    return $ length (fsCounts fs)
    
freqSumCombine :: Int -> Int -> (Maybe FreqSumEntry, Maybe FreqSumEntry) -> FreqSumEntry
freqSumCombine _ n2 (Just fs1, Nothing) = fs1 {fsCounts = fsCounts fs1 ++ replicate n2 0}
freqSumCombine n1 _ (Nothing, Just fs2) = fs2 {fsCounts = replicate n1 0 ++ fsCounts fs2}
freqSumCombine _ n2 (Just fs1, Just fs2) =
    if fsChrom fs1 == fsChrom fs2 && fsRef fs1 == fsRef fs2 && fsAlt fs1 == fsAlt fs2
        then fs1 {fsCounts = fsCounts fs1 ++ fsCounts fs2}
        else fs1 {fsCounts = fsCounts fs1 ++ replicate n2 0}
freqSumCombine _ _ (Nothing, Nothing) = undefined