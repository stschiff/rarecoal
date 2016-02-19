{-# LANGUAGE BangPatterns #-}

import Rarecoal.Core (choose)
import Rarecoal.RareAlleleHistogram (readHistogramFromHandle, showHistogram, 
                                     RareAlleleHistogram(..), SitePattern(..))

import Control.Error (runScript, tryRight, errLn)
import Control.Foldl (impurely, FoldM(..))
import Control.Lens ((&), (%~), ix)
import Control.Monad (replicateM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import qualified Data.Text.IO as T
import qualified Options.Applicative as OP
import Pipes (yield, for, Producer)
import qualified Pipes.Prelude as P
import System.IO (stdin, IOMode(..), openFile)
import System.Random (randomIO)

data MyOpts = MyOpts {
    _optQueryPop :: Int,
    _optHowMany :: Int,
    _optHistPath :: FilePath
}

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "sample a number of haplotypes (independently at each site) from a population in a histogram")

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.option OP.auto (OP.short 'q' <> OP.long "queryBranch" <> OP.metavar "<INT>" <> OP.help "the population index (0-based) from which to sample")
                <*> OP.option OP.auto (OP.short 'n' <> OP.long "howMany" <> OP.metavar "<INT>" <> OP.help "how many samples should be drawn at each site")
                <*> OP.option OP.str (OP.short 'i' <> OP.long "hist" <> OP.metavar "<path-to-histogram>" <> OP.help "the input histogram file, set - for stdin")

runWithOptions :: MyOpts -> IO ()
runWithOptions opts = do
    handle <- if _optHistPath opts == "-" then
            return stdin
        else
            openFile (_optHistPath opts) ReadMode
    hist <- runScript $ readHistogramFromHandle handle
    hist' <- makeNewHist (_optQueryPop opts) (_optHowMany opts) hist
    outs <- runScript . tryRight $ showHistogram hist'
    T.putStr outs

makeNewHist :: Int -> Int -> RareAlleleHistogram -> IO RareAlleleHistogram
makeNewHist query howMany hist = do
    let histRows = M.toList (raCounts hist)
        nVec = raNVec hist
        patternProducer = mapM_ yield histRows
        sampledProducer = for patternProducer (sampleFromPattern query howMany nVec)
    (patternMap, _) <- impurely P.foldM' makeMap sampledProducer
    let newNVec = (nVec & ix query %~ (\v -> v - howMany)) ++ [howMany]
    return hist {raNVec = newNVec, raCounts = patternMap}

sampleFromPattern :: Int -> Int -> [Int] -> (SitePattern, Int64) ->
                     Producer (SitePattern, Int64) IO ()
sampleFromPattern query howMany nVec (pat, count) = do
    liftIO $ errLn ("processing pattern " ++ show pat)
    case pat of
        Higher -> yield (Higher, count)
        Pattern pattern -> do
            let n = nVec !! query
                k = pattern !! query
            if k == 0 then
                yield (Pattern (pattern ++ [0]), count)
            else
                replicateM_ (fromIntegral count) $ do
                    newK <- liftIO $ sampleWithoutReplacement n k howMany
                    let newPat = (pattern & ix query %~ (\v -> v - newK)) ++ [newK]
                    yield (Pattern newPat, 1)

sampleWithoutReplacement :: Int -> Int -> Int -> IO Int
sampleWithoutReplacement n k howMany = go n k howMany 0
  where
    go !_ !_ !0 !ret = return ret
    go !_ !0 !_ !ret = return ret
    go !n' !k' !howMany' !ret = do
        let noSuccess = choose (n' - k') howMany' / choose n' howMany'
        val <- bernoulli noSuccess
        if val then
            return ret
        else do
            go (n' - 1) (k' - 1) (howMany' - 1) (ret + 1)
        
bernoulli :: Double -> IO Bool
bernoulli p = (<p) <$> randomIO 

makeMap :: FoldM IO (SitePattern, Int64) (M.Map SitePattern Int64)
makeMap = FoldM step initial extract
  where
    step m (p, c) = return $ M.insertWith (+) p c m
    initial = return M.empty
    extract = return 
