module Mcmc (runMcmc, reportMcmcResult, reportMcmcTrace, readMaxResult) where

import RareAlleleHistogram (RareAlleleHistogram(..))
import ModelSpec (ModelTemplate(..), instantiateModel)
import qualified Data.Vector.Unboxed as V
import qualified System.Random as R
import Logl (computeLikelihood)
import Maxl (minFunc, validateModel)
import Control.Monad.Trans.State.Lazy (StateT, get, gets, put, evalStateT, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, replicateM)
import Control.Error (EitherT(..))

(!) = (V.!)
(//) = (V.//)

data MCMCstate = MCMCstate {
    mcmcNrSteps :: Int,
    mcmcCurrentValue :: Double,
    mcmcCurrentPoint :: V.Vector Double,
    mcmcStepWidths :: V.Vector Double,
    mcmcSuccesRate :: V.Vector Double,
    mcmcRanGen :: R.StdGen
} deriving (Show)

runMcmc :: ModelTemplate -> V.Vector Double -> RareAlleleHistogram -> Int -> Int -> EitherT String IO ([[Double]], [V.Vector Double])
runMcmc modelTemplate params hist cycles seed = do
    let initialModel = instantiateModel modelTemplate params
    _ <- lift . return $ validateModel initialModel
    _ <- lift . return $ computeLikelihood initialModel hist
    let minFunc' = minFunc modelTemplate hist
        initV = minFunc' params
        stepWidths = V.map (/100.0) params
        successRates = V.replicate (V.length params) 0.44
        ranGen = R.mkStdGen seed 
        initState = MCMCstate 0 initV params stepWidths successRates ranGen
    states <- liftIO $ evalStateT (replicateM cycles (mcmcCycle minFunc')) initState
    return ([], map mcmcCurrentPoint states)

mcmcCycle :: (V.Vector Double -> Double) -> StateT MCMCstate IO MCMCstate
mcmcCycle posterior = do
    state <- get
    let k = V.length $ mcmcCurrentPoint state
        rng = mcmcRanGen state
        (order, rng') = shuffle rng [0..k-1]
    modify (\s -> s {mcmcRanGen = rng'})
    mapM_ (updateMCMC posterior) order
    get >>= return

shuffle :: R.StdGen -> [Int] -> ([Int], R.StdGen)
shuffle rng r = (r, rng) 

updateMCMC :: (V.Vector Double -> Double) -> Int -> StateT MCMCstate IO MCMCstate
updateMCMC posterior i = do
    newPoint <- propose i
    let newVal = posterior newPoint
    success <- isSuccessFul newVal
    if success
        then accept newPoint newVal i
        else reject i
    adaptStepWidths i
    state <- get
    let steps = mcmcNrSteps state
        newState = state {mcmcNrSteps = steps + 1}
    when (steps `mod` 10 == 0) $ liftIO (print state)
    put newState
    return newState
    
propose :: Int -> StateT MCMCstate IO (V.Vector Double)
propose i = do
    state <- get
    let currentPoint = mcmcCurrentPoint state
        rng = mcmcRanGen state
        d = (mcmcStepWidths state)!i
        (ran, rng') = R.randomR (-d, d) rng
        newPoint = currentPoint // [(i, currentPoint!i + ran)]
    put state {mcmcRanGen = rng'}
    return newPoint

isSuccessFul :: Double -> StateT MCMCstate IO Bool
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

accept :: V.Vector Double -> Double -> Int -> StateT MCMCstate IO ()
accept newPoint newVal i = do
    successRate <- gets mcmcSuccesRate
    let newR = successRate!i * 0.99 + 0.01
        successRate' = successRate // [(i, newR)]
    modify (\s -> s {mcmcCurrentPoint = newPoint, mcmcCurrentValue = newVal, mcmcSuccesRate = successRate'})

reject :: Int -> StateT MCMCstate IO ()
reject i = do
    successRate <- gets mcmcSuccesRate
    let newR = successRate!i * 0.99
        successRate' = successRate // [(i, newR)]
    modify (\s -> s {mcmcSuccesRate = successRate'})

adaptStepWidths :: Int -> StateT MCMCstate IO ()
adaptStepWidths i = do
    state <- get
    let r = (mcmcSuccesRate state)!i
    when (r < 0.29 || r > 0.59) $ do
        let d = (mcmcStepWidths state)!i
            d' = if r < 0.29 then d / 1.5 else d * 1.5
            newStepWidths = (mcmcStepWidths state) // [(i, d')]
        put state {mcmcStepWidths = newStepWidths}

reportMcmcResult :: ModelTemplate -> [[Double]] -> IO ()
reportMcmcResult modelTemplate mcmcResult = undefined

reportMcmcTrace :: ModelTemplate -> [V.Vector Double] -> FilePath -> IO ()
reportMcmcTrace modelTemplate trace path = undefined

readMaxResult :: FilePath -> IO [Double]
readMaxResult maxResultPath = do
    c <- readFile maxResultPath
    return [read (w!!1) | w <- map words . lines $ c] 
