module Mcmc (runMcmc, McmcOpt(..)) where

import ModelTemplate (ModelTemplate(..), instantiateModel, readModelTemplate)
import Core (defaultTimes)
import qualified Data.Vector.Unboxed as V
import qualified System.Random as R
import Maxl (minFunc, validateModel)
import Control.Monad.Trans.State.Lazy (StateT, get, gets, put, evalStateT, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, replicateM, forM_)
import Data.List (intercalate, sort, minimumBy)
import Control.Error (Script, scriptIO)
import RareAlleleHistogram (loadHistogram)
import Control.Monad.Trans.Either (hoistEither)
import Data.Int (Int64)
import Data.Ord (comparing)

(!) = (V.!)
(//) = (V.//)

data McmcOpt = McmcOpt {
   mcTheta :: Double,
   mcTemplatePath :: FilePath,
   mcInitialParams :: [Double],
   mcNrBurninCycles :: Int,
   mcNrMainCycles :: Int,
   mcTracePath :: FilePath,
   mcMaxAf :: Int,
   mcNrCalledSites :: Int64,
   mcHistPath :: FilePath,
   mcRandomSeed :: Int
}

data MCMCstate = MCMCstate {
    mcmcNrSteps :: Int,
    mcmcNrCycles :: Int,
    mcmcCurrentValue :: Double,
    mcmcCurrentPoint :: V.Vector Double,
    mcmcStepWidths :: V.Vector Double,
    mcmcSuccesRate :: V.Vector Double,
    mcmcRanGen :: R.StdGen
} deriving (Show)

runMcmc :: McmcOpt -> Script ()
runMcmc opts = do
    modelTemplate <- scriptIO $ readModelTemplate (mcTemplatePath opts) (mcTheta opts) defaultTimes
    hist <- loadHistogram (mcMaxAf opts) (mcNrCalledSites opts) (mcHistPath opts)
    modelSpec <- hoistEither $ instantiateModel modelTemplate (V.fromList $ mcInitialParams opts)
    hoistEither $ validateModel modelSpec
    let minFunc' = minFunc modelTemplate hist
        params = V.fromList $ mcInitialParams opts
    initV <- hoistEither $ minFunc' params
    let stepWidths = V.map (\p -> max 1.0e-8 $ (abs p) / 100.0) params
        successRates = V.replicate (V.length params) 0.44
        ranGen = R.mkStdGen $ mcRandomSeed opts 
        initState = MCMCstate 0 0 initV params stepWidths successRates ranGen
        nrCycles = mcNrBurninCycles opts + mcNrMainCycles opts
    states <- evalStateT (replicateM nrCycles (mcmcCycle minFunc')) initState
    scriptIO $ reportPosteriorStats (mcNrBurninCycles opts) (mtParams modelTemplate) states
    scriptIO $ reportTrace (mtParams modelTemplate) states (mcTracePath opts)

mcmcCycle :: (V.Vector Double -> Either String Double) -> StateT MCMCstate Script MCMCstate
mcmcCycle posterior = do
    state <- get
    let k = V.length $ mcmcCurrentPoint state
        rng = mcmcRanGen state
        (order, rng') = shuffle rng [0..k-1]
    modify (\s -> s {mcmcRanGen = rng'})
    mapM_ (updateMCMC posterior) order
    let c = mcmcNrCycles state
    modify (\s -> s {mcmcNrCycles = c + 1})
    when ((c + 1) `mod` 10 == 0) $ do
        liftIO (print state)
        forM_ [0..k-1] adaptStepWidths
    get >>= return

shuffle :: R.StdGen -> [Int] -> ([Int], R.StdGen)
shuffle rng [] = ([], rng)
shuffle rng r =
    let (ran, rng') = R.randomR (0, length r - 1) rng
        val = r!!ran
        (rest, rng'') = shuffle rng' [v | (v, i) <- zip r [0..], i /= ran]
    in  ((val:rest), rng'')

updateMCMC :: (V.Vector Double -> Either String Double) -> Int -> StateT MCMCstate Script MCMCstate
updateMCMC posterior i = do
    newPoint <- propose i
    newVal <- lift . hoistEither . posterior $ newPoint
    success <- isSuccessFul newVal
    if success
        then accept newPoint newVal i
        else reject i
    state <- get
    let steps = mcmcNrSteps state
        newState = state {mcmcNrSteps = steps + 1}
    put newState
    return newState
    
propose :: Int -> StateT MCMCstate Script (V.Vector Double)
propose i = do
    state <- get
    let currentPoint = mcmcCurrentPoint state
        rng = mcmcRanGen state
        d = (mcmcStepWidths state)!i
        (ran, rng') = R.randomR (-d, d) rng
        newPoint = currentPoint // [(i, currentPoint!i + ran)]
    put state {mcmcRanGen = rng'}
    return newPoint

isSuccessFul :: Double -> StateT MCMCstate Script Bool
isSuccessFul newVal = do
    state <- get
    if newVal < mcmcCurrentValue state
        then return True
        else do
            let rng = mcmcRanGen state
                (ran, rng') = R.randomR (0.0, 1.0) rng
            put state {mcmcRanGen = rng'}
            if ran < exp(mcmcCurrentValue state - newVal)
                then return True
                else return False

accept :: V.Vector Double -> Double -> Int -> StateT MCMCstate Script ()
accept newPoint newVal i = do
    successRate <- gets mcmcSuccesRate
    let newR = successRate!i * 0.975 + 0.025
        successRate' = successRate // [(i, newR)]
    modify (\s -> s {mcmcCurrentPoint = newPoint, mcmcCurrentValue = newVal, mcmcSuccesRate = successRate'})

reject :: Int -> StateT MCMCstate Script ()
reject i = do
    successRate <- gets mcmcSuccesRate
    let newR = successRate!i * 0.975
        successRate' = successRate // [(i, newR)]
    modify (\s -> s {mcmcSuccesRate = successRate'})

adaptStepWidths :: Int -> StateT MCMCstate Script ()
adaptStepWidths i = do
    state <- get
    let r = (mcmcSuccesRate state)!i
    when (r < 0.29 || r > 0.59) $ do
        let d = (mcmcStepWidths state)!i
            d' = if r < 0.29 then d / 1.5 else d * 1.5
            newStepWidths = (mcmcStepWidths state) // [(i, d')]
        put state {mcmcStepWidths = newStepWidths}

reportPosteriorStats :: Int -> [String] -> [MCMCstate] -> IO ()
reportPosteriorStats nrBurninCycles paramNames states = do
    let dim = V.length $ mcmcCurrentPoint (head states)
        points = map mcmcCurrentPoint . drop nrBurninCycles $ states
        scores = map mcmcCurrentValue . drop nrBurninCycles $ states
        minIndex = snd $ minimumBy (comparing fst) $ zip scores [0..]
        orderStats = [getOrderStats minIndex $ map (!i) points | i <- [0..dim-1]]
        orderStatsScore = getOrderStats minIndex scores
        paramLines = zipWith (\n s -> intercalate "\t" $ (n:map show s)) paramNames orderStats
        scoreLine = intercalate "\t" ("Score":map show orderStatsScore)
        headerLine = "Param\tMaxL\tMedian\tLowerCI\tUpperCI"
    putStr $ unlines (headerLine:scoreLine:paramLines)
  where
    getOrderStats minI vals =
        let nPoints = fromIntegral $ length vals :: Double
            [lowCIindex, midIndex, highCIindex] = map (floor . (*nPoints)) [0.025, 0.5, 0.975]
            sortedVals = sort vals
        in  [vals!!minI] ++ map (sortedVals!!) [lowCIindex, midIndex, highCIindex] 
    
reportTrace :: [String] -> [MCMCstate] -> FilePath -> IO ()
reportTrace paramNames states traceFilePath = do
    let body = [intercalate "\t" . map show . V.toList $ V.concat [V.singleton (mcmcCurrentValue s), mcmcCurrentPoint s, mcmcStepWidths s, mcmcSuccesRate s] | s <- states]
        headerLine = intercalate "\t" $ ["Score"] ++ paramNames ++ map (++"_delta") paramNames ++ map (++"_success") paramNames
    writeFile traceFilePath $ unlines (headerLine:body)
