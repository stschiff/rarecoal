{-# LANGUAGE TemplateHaskell #-}

import qualified Options.Applicative as OP
import Control.Monad.Trans.State.Strict (evalState, State, get, put)
import Control.Error (runScript, scriptIO, tryRight, throwE)
import Control.Lens ((&), (%~), ix) 
import Rarecoal.RareAlleleHistogram (readHistogramFromHandle, showHistogram, RareAlleleHistogram(..), SitePattern(..))
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import Data.Int (Int64)
import Data.Foldable (foldl')
import System.Random (newStdGen, StdGen, random)
import Control.Monad (replicateM, when)
import qualified Data.Text.IO as T
import System.IO (stdin, IOMode(..), openFile)

data MyOpts = MyOpts {
    _optQueryPop :: Int,
    _optHowMany :: Int,
    _optHistPath :: FilePath
}

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) mempty

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.option OP.auto (OP.short 'q' <> OP.long "queryBranch" <> OP.metavar "<INT>")
                <*> OP.option OP.auto (OP.short 'n' <> OP.long "howMany" <> OP.metavar "<INT>")
                <*> OP.option OP.str (OP.short 'i' <> OP.long "hist" <> OP.metavar "<path-to-histogram>")

runWithOptions :: MyOpts -> IO ()
runWithOptions opts = runScript $ do
    handle <- if _optHistPath opts == "-" then return stdin else scriptIO $ openFile (_optHistPath opts) ReadMode
    hist <- readHistogramFromHandle handle
    when (raGlobalMax hist) $ throwE "histogram cannot have global max for this operation"
    rng <- scriptIO newStdGen
    let hist' = evalState (addSamplePop (_optQueryPop opts) (_optHowMany opts) hist) rng
    outs <- tryRight $ showHistogram hist'
    scriptIO $ T.putStr outs

addSamplePop :: Int -> Int -> RareAlleleHistogram -> State StdGen RareAlleleHistogram
addSamplePop query howMany hist = do
    let histRows = Map.toList (raCounts hist)
        nVec = raNVec hist
    patternMaps <- sequence [sampleFromPatterns query howMany nVec pat count | (pat, count) <- histRows]
    let newBody = Map.unionsWith (+) patternMaps
        newNVec = (nVec & ix query %~ (\v -> v - howMany)) ++ [howMany]
    return hist {raNVec = newNVec, raCounts = newBody}

sampleFromPatterns :: Int -> Int -> [Int] -> SitePattern -> Int64 -> State StdGen (Map.Map SitePattern Int64)
sampleFromPatterns _ _ _ Higher count = return $ Map.singleton Higher count
sampleFromPatterns query howMany nVec pattern@(Pattern p) count =
    if p !! query == 0 then
        return $ Map.singleton (Pattern $ p ++ [0]) count
    else do
        patterns <- replicateM (fromIntegral count) $ sampleFromPattern query howMany nVec pattern
        return $ foldl' insertPattern Map.empty patterns
  where
    insertPattern m p = Map.insertWith (\_ v -> v + 1) p 1 m

sampleFromPattern :: Int -> Int -> [Int] -> SitePattern -> State StdGen SitePattern
sampleFromPattern _ _ _ Higher = return Higher
sampleFromPattern query howMany nVec (Pattern pattern) = do
    let n = nVec !! query
        k = pattern !! query
    newK <- sampleWithoutReplacement n k howMany
    return $ Pattern ((pattern & ix query %~ (\v -> v - newK)) ++ [newK])
    
sampleWithoutReplacement :: Int -> Int -> Int -> State StdGen Int 
sampleWithoutReplacement n k howMany = go n k howMany 0
  where
    go _ _ 0 ret = return ret
    go _ 0 _ ret = return ret
    go n k howMany ret = do
        val <- bernoulli $ fromIntegral k / fromIntegral n
        if val then go (n - 1) (k - 1) (howMany - 1) (ret + 1) else go (n - 1) k (howMany - 1) ret
    
bernoulli :: Double -> State StdGen Bool
bernoulli p = do
    rng <- get
    let (ran, rng') = random rng
    put rng'
    return $ ran < p
    
