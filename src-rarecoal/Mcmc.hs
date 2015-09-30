module Mcmc (runMcmc, McmcOpt(..)) where

import Maxl (minFunc, penalty)
import ModelTemplate (ModelTemplate(..), readModelTemplate, getInitialParams)
import Rarecoal.Core (getTimeSteps, ModelEvent(..), EventType(..)) 
import Rarecoal.RareAlleleHistogram (loadHistogram)

import qualified Data.Vector.Unboxed as V
import qualified System.Random as R
import Control.Monad.Trans.State.Lazy (StateT, get, gets, put, evalStateT, modify)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, forM_)
import Data.List (intercalate, sort, minimumBy)
import Control.Error (Script, scriptIO, tryRight)
import Data.Int (Int64)
import Data.Ord (comparing)
import System.Log.Logger (infoM)
import Control.Monad.Loops (whileM)

(!) :: V.Vector Double -> Int -> Double 
(!) = (V.!)
(//) :: V.Vector Double -> [(Int, Double)] -> V.Vector Double
(//) = (V.//)

data McmcOpt = McmcOpt {
   mcTheta :: Double,
   mcTemplatePath :: FilePath,
   mcInitialParamsFile :: FilePath,
   mcInitialParams :: [Double],
   mcNrCycles :: Int,
   mcTracePath :: FilePath,
   mcMinAf :: Int,
   mcMaxAf :: Int,
   mcConditionOn :: [Int],
   mcNrCalledSites :: Int64,
   mcLinGen :: Int,
   mcIndices :: [Int],
   mcHistPath :: FilePath,
   mcRandomSeed :: Int,
   mcBranchAges :: [Double]
}

data MCMCstate = MCMCstate {
    mcmcNrSteps :: Int,
    mcmcNrCycles :: Int,
    mcmcCurrentValue :: Double,
    mcmcCurrentPoint :: V.Vector Double,
    mcmcStepWidths :: V.Vector Double,
    mcmcSuccesRate :: V.Vector Double,
    mcmcRanGen :: R.StdGen,
    mcmcScoreList :: [Double]
} deriving (Show)

runMcmc :: McmcOpt -> Script ()
runMcmc opts = do
    let times = getTimeSteps 20000 (mcLinGen opts) 20.0
    modelTemplate <- readModelTemplate (mcTemplatePath opts) (mcTheta opts) times
    hist <- loadHistogram (mcIndices opts) (mcMinAf opts) (mcMaxAf opts) (mcConditionOn opts) (mcNrCalledSites opts) (mcHistPath opts)
    let extraEvents = concat [[ModelEvent 0.0 (SetFreeze k True), ModelEvent t (SetFreeze k False)] | (t, k) <- zip (mcBranchAges opts) [0..], t > 0]
    x <- getInitialParams modelTemplate (mcInitialParamsFile opts) (mcInitialParams opts)
    _ <- tryRight $ minFunc modelTemplate extraEvents hist x
    let minFunc' = either (const penalty) id . minFunc modelTemplate extraEvents hist
        initV = minFunc' x
        stepWidths = V.map (max 1.0e-8 . abs . (/100.0)) x
        successRates = V.replicate (V.length x) 0.44
    ranGen <- if mcRandomSeed opts == 0 then scriptIO R.getStdGen else return $ R.mkStdGen (mcRandomSeed opts)
    let initState = MCMCstate 0 0 initV x stepWidths successRates ranGen []
        pred_ = mcmcNotDone (mcNrCycles opts)
        act = mcmcCycle minFunc'
    states <- evalStateT (whileM pred_ act) initState
    scriptIO $ reportPosteriorStats (mtParams modelTemplate) states
    scriptIO $ reportTrace (mtParams modelTemplate) states (mcTracePath opts)

mcmcNotDone :: Int -> StateT MCMCstate Script Bool
mcmcNotDone requiredCycles = do
    state <- get
    let nrCycles = mcmcNrCycles state
    -- return $ nrCycles < 5
    if nrCycles < requiredCycles then return True else
        let scoreValues = reverse $ mcmcScoreList state
            nrBurnins = computeBurninCycles scoreValues
        in  return $ nrCycles - nrBurnins <= requiredCycles

computeBurninCycles :: [Double] -> Int
computeBurninCycles s =
    let scoreBound = minimum s + 10.0
    in  fst . head . filter ((<scoreBound) . snd) $ zip [0..] s

mcmcCycle :: (V.Vector Double -> Double) -> StateT MCMCstate Script MCMCstate
mcmcCycle posterior = do
    state <- get
    let c = mcmcNrCycles state
        scoreValues = mcmcScoreList state
        k = V.length $ mcmcCurrentPoint state
        rng = mcmcRanGen state
        (order, rng') = shuffle rng [0..k-1]
    modify (\s -> s {mcmcRanGen = rng'})
    mapM_ (updateMCMC posterior) order
    newVal <- gets mcmcCurrentValue
    modify (\s -> s {mcmcNrCycles = c + 1, mcmcScoreList = newVal:scoreValues})
    -- liftIO (infoM "rarecoal" $ showStateLog state)
    when ((c + 1) `mod` 10 == 0) $ do
        liftIO (infoM "rarecoal" $ showStateLog state)
        forM_ [0..k-1] adaptStepWidths
    get 

shuffle :: R.StdGen -> [Int] -> ([Int], R.StdGen)
shuffle rng [] = ([], rng)
shuffle rng r =
    let (ran, rng') = R.randomR (0, length r - 1) rng
        val = r!!ran
        (rest, rng'') = shuffle rng' [v | (v, i) <- zip r [0..], i /= ran]
    in  (val:rest, rng'')

updateMCMC :: (V.Vector Double -> Double) -> Int -> StateT MCMCstate Script MCMCstate
updateMCMC posterior i = do
    newPoint <- propose i
    let newVal = posterior newPoint
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
        d = mcmcStepWidths state!i
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
            return $ ran < exp (mcmcCurrentValue state - newVal)

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
    let r = mcmcSuccesRate state!i
    when (r < 0.29 || r > 0.59) $ do
        let d = mcmcStepWidths state!i
            d' = if r < 0.29 then d / 1.5 else d * 1.5
            newStepWidths = mcmcStepWidths state // [(i, d')]
        put state {mcmcStepWidths = newStepWidths}

showStateLog :: MCMCstate -> String
showStateLog state = 
    "Cycle: " ++ show (mcmcNrCycles state) ++
    "; Value: " ++ show (mcmcCurrentValue state) ++
    "; Point: " ++ show (mcmcCurrentPoint state)

reportPosteriorStats :: [String] -> [MCMCstate] -> IO ()
reportPosteriorStats paramNames states = do
    let dim = V.length $ mcmcCurrentPoint (head states)
        allScores = map mcmcCurrentValue states
        nrBurninCycles = computeBurninCycles allScores 
        points = map mcmcCurrentPoint . drop nrBurninCycles $ states
        scores = drop nrBurninCycles allScores 
        minIndex = snd $ minimumBy (comparing fst) $ zip scores [0..]
        orderStats = [getOrderStats minIndex $ map (!i) points | i <- [0..dim-1]]
        orderStatsScore = getOrderStats minIndex scores
        paramLines = zipWith (\n s -> intercalate "\t" (n:map show s)) paramNames orderStats
        scoreLine = intercalate "\t" ("Score":map show orderStatsScore)
        headerLine = "Param\tMaxL\tLowerCI\tMedian\tUpperCI"
    putStrLn $ "# Nr Burnin Cycles: " ++ show nrBurninCycles
    putStrLn $ "# Nr Main Cycles: " ++ show (length states - nrBurninCycles)
    putStr $ unlines (headerLine:scoreLine:paramLines)
  where
   getOrderStats minI vals =
        let nPoints = fromIntegral $ length vals :: Double
            [lowCIindex, midIndex, highCIindex] = map (floor . (*nPoints)) [0.025, 0.5, 0.975]
            sortedVals = sort vals
        in  vals!!minI : map (sortedVals!!) [lowCIindex, midIndex, highCIindex] 
    
reportTrace :: [String] -> [MCMCstate] -> FilePath -> IO ()
reportTrace paramNames states traceFilePath = do
    let body = [intercalate "\t" . map show . V.toList $
                V.concat [V.singleton (mcmcCurrentValue s), mcmcCurrentPoint s, mcmcStepWidths s, mcmcSuccesRate s] |
                s <- states]
        headerLine = intercalate "\t" $ ["Score"] ++ paramNames ++ map (++"_delta") paramNames ++
                     map (++"_success") paramNames
    writeFile traceFilePath $ unlines (headerLine:body)
