{-# LANGUAGE OverloadedStrings #-}
module Mcmc (runMcmc, McmcOpt(..)) where

import           RarecoalLib.MaxUtils           (computeFrequencySpectrum,
                                                 makeInitialPoint, minFunc,
                                                 penalty, writeFullFitTable,
                                                 writeSummaryFitTable)
import           RarecoalLib.ModelTemplate      (ModelOptions (..),
                                                 ModelTemplate (..),
                                                 ParamOptions (..),
                                                 fillParameterDictWithDefaults,
                                                 getModelTemplate,
                                                 getParamNames,
                                                 instantiateModel,
                                                 makeParameterDict)
import           RarecoalLib.Utils              (GeneralOptions (..),
                                                 HistogramOptions (..),
                                                 setNrProcessors, tryEither)
import           RarecoalLib.Utils              (loadHistogram)

import           Control.Monad                  (forM_, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Loops            (whileM)
import           Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, gets,
                                                 modify, put)
import           Data.List                      (intercalate, minimumBy, sort)
import           Data.Ord                       (comparing)
import qualified Data.Vector.Unboxed            as V
import           System.IO                      (Handle, IOMode (..), hPutStr,
                                                 hPutStrLn, stderr, withFile)
import qualified System.Random                  as R

(!) :: V.Vector Double -> Int -> Double
(!) = (V.!)
(//) :: V.Vector Double -> [(Int, Double)] -> V.Vector Double
(//) = (V.//)

data McmcOpt = McmcOpt
    { mcGeneralOpts   :: GeneralOptions
    , mcModelOpts     :: ModelOptions
    , mcParamOpts     :: ParamOptions
    , mcHistogramOpts :: HistogramOptions
    , mcNrCycles      :: Int
    , mcOutPrefix     :: FilePath
    , mcRandomSeed    :: Int
    }

data MCMCstate = MCMCstate
    { mcmcNrSteps      :: Int
    , mcmcNrCycles     :: Int
    , mcmcCurrentValue :: Double
    , mcmcCurrentPoint :: V.Vector Double
    , mcmcStepWidths   :: V.Vector Double
    , mcmcSuccesRate   :: V.Vector Double
    , mcmcRanGen       :: R.StdGen
    , mcmcScoreList    :: [Double]
    }
    deriving (Show)

runMcmc :: McmcOpt -> IO ()
runMcmc opts = do
    setNrProcessors (mcGeneralOpts opts)
    modelTemplate <- getModelTemplate (mcModelOpts opts)
    modelParams <- (makeParameterDict (mcParamOpts opts)) >>=
        fillParameterDictWithDefaults modelTemplate
    _ <- tryEither $ instantiateModel (mcGeneralOpts opts) modelTemplate modelParams
    let modelBranchNames = mtBranchNames modelTemplate
    (hist, siteRed) <- loadHistogram (mcHistogramOpts opts) modelBranchNames
    xInit <- tryEither $ makeInitialPoint modelTemplate modelParams
    _ <- tryEither $ minFunc (mcGeneralOpts opts) modelTemplate hist siteRed xInit
    let minFunc' = either (const penalty) id .
            minFunc (mcGeneralOpts opts) modelTemplate hist siteRed
        initV = minFunc' xInit
        stepWidths = V.map (max 1.0e-8 . abs . (/100.0)) xInit
        successRates = V.replicate (V.length xInit) 0.44
    ranGen <- if   mcRandomSeed opts == 0
              then R.getStdGen
              else return $ R.mkStdGen (mcRandomSeed opts)
    let initState = MCMCstate 0 0 initV xInit stepWidths successRates ranGen []
        pred_ = mcmcNotDone (mcNrCycles opts)
        act = mcmcCycle minFunc' (getParamNames modelTemplate)
    states <- evalStateT (whileM pred_ act) initState

    let outMcmcFN = mcOutPrefix opts ++ ".paramEstimates.txt"
        outTraceFN = mcOutPrefix opts ++ ".trace.txt"
        outFullFitTableFN = mcOutPrefix opts ++ ".frequencyFitTable.txt"
        outSummaryTableFN = mcOutPrefix opts ++ ".summaryFitTable.txt"
    minPoint <- withFile outMcmcFN WriteMode $ \h ->
        reportPosteriorStats (getParamNames modelTemplate) states h
    withFile outTraceFN WriteMode $ \h ->
        reportTrace (getParamNames modelTemplate) states h

    let finalModelParams = [(n, r) | ((n, _), r) <- zip modelParams (V.toList minPoint)]
    finalModelSpec <- tryEither $ instantiateModel (mcGeneralOpts opts) modelTemplate finalModelParams
    finalSpectrum <- tryEither $ computeFrequencySpectrum finalModelSpec hist modelBranchNames
    writeFullFitTable outFullFitTableFN finalSpectrum
    writeSummaryFitTable outSummaryTableFN finalSpectrum hist


mcmcNotDone :: Int -> StateT MCMCstate IO Bool
mcmcNotDone requiredCycles = do
    state <- get
    let nrCycles = mcmcNrCycles state
    -- return $ nrCycles < 5
    if nrCycles < requiredCycles then return True else
        let scoreValues = reverse $ mcmcScoreList state
            nrBurnins = computeBurninCycles scoreValues
        in  return $ nrCycles - nrBurnins <= requiredCycles

computeBurninCycles :: [Double] -> Int
computeBurninCycles vals =
    let scoreBound = minimum vals + 10.0
    in  fst . head . filter ((<scoreBound) . snd) $ zip [0..] vals

mcmcCycle :: (V.Vector Double -> Double) -> [String] -> StateT MCMCstate IO MCMCstate
mcmcCycle posterior paramNames = do
    state <- get
    let c = mcmcNrCycles state
        scoreValues = mcmcScoreList state
        k = V.length $ mcmcCurrentPoint state
        rng = mcmcRanGen state
        (order, rng') = shuffle rng [0..k-1]
    modify (\st -> st {mcmcRanGen = rng'})
    mapM_ (updateMCMC posterior) order
    newVal <- gets mcmcCurrentValue
    modify (\st -> st {mcmcNrCycles = c + 1, mcmcScoreList = newVal:scoreValues})
    -- liftIO (errLn $ showStateLog state paramNames)
    when ((c + 1) `mod` 10 == 0) $ do
        showStateLog paramNames
        forM_ [0..k-1] adaptStepWidths
    get

shuffle :: R.StdGen -> [Int] -> ([Int], R.StdGen)
shuffle rng [] = ([], rng)
shuffle rng r =
    let (ran, rng') = R.randomR (0, length r - 1) rng
        val = r!!ran
        (rest, rng'') = shuffle rng' [v | (v, i) <- zip r [0..], i /= ran]
    in  (val:rest, rng'')

updateMCMC :: (V.Vector Double -> Double) -> Int -> StateT MCMCstate IO MCMCstate
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

propose :: Int -> StateT MCMCstate IO (V.Vector Double)
propose i = do
    state <- get
    let currentPoint = mcmcCurrentPoint state
        rng = mcmcRanGen state
        delta = mcmcStepWidths state!i
        (ran, rng') = R.randomR (-delta, delta) rng
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
            return $ ran < exp (mcmcCurrentValue state - newVal)

accept :: V.Vector Double -> Double -> Int -> StateT MCMCstate IO ()
accept newPoint newVal i = do
    successRate <- gets mcmcSuccesRate
    let newR = successRate!i * 0.975 + 0.025
        successRate' = successRate // [(i, newR)]
    modify (\st -> st {mcmcCurrentPoint = newPoint, mcmcCurrentValue = newVal,
                     mcmcSuccesRate = successRate'})

reject :: Int -> StateT MCMCstate IO ()
reject i = do
    successRate <- gets mcmcSuccesRate
    let newR = successRate!i * 0.975
        successRate' = successRate // [(i, newR)]
    modify (\st -> st {mcmcSuccesRate = successRate'})

adaptStepWidths :: Int -> StateT MCMCstate IO ()
adaptStepWidths i = do
    state <- get
    let r = mcmcSuccesRate state!i
    when (r < 0.29 || r > 0.59) $ do
        let delta = mcmcStepWidths state!i
            delta' = if r < 0.29 then delta / 1.5 else delta * 1.5
            newStepWidths = mcmcStepWidths state // [(i, delta')]
        put state {mcmcStepWidths = newStepWidths}

showStateLog :: [String] -> StateT MCMCstate IO ()
showStateLog paramNames = do
    state <- get
    liftIO . hPutStrLn stderr $ "Cycle=" ++ show (mcmcNrCycles state) ++ "\tValue=" ++ show (mcmcCurrentValue state) ++
        "\t" ++ params state
  where
    params state = intercalate "\t" [key ++ "=" ++ show val |
        (key, val) <- zip paramNames (V.toList $ mcmcCurrentPoint state)]

reportPosteriorStats :: [String] -> [MCMCstate] -> Handle -> IO (V.Vector Double)
reportPosteriorStats paramNames states h = do
    let dim = V.length $ mcmcCurrentPoint (head states)
        allScores = map mcmcCurrentValue states
        nrBurninCycles = computeBurninCycles allScores
        points = map mcmcCurrentPoint . drop nrBurninCycles $ states
        scores = drop nrBurninCycles allScores
        minIndex = snd $ minimumBy (comparing fst) $ zip scores [0..]
        orderStats = [getOrderStats minIndex $ map (!i) points | i <- [0..dim-1]]
        orderStatsScore = getOrderStats minIndex scores
        paramLines = zipWith (\n st -> intercalate "\t" (n:map show st)) paramNames orderStats
        scoreLine = intercalate "\t" ("Score":map show orderStatsScore)
        headerLine = "Param\tMaxL\tLowerCI\tMedian\tUpperCI"
    hPutStrLn h $ "# Nr Burnin Cycles: " ++ show nrBurninCycles
    hPutStrLn h $ "# Nr Main Cycles: " ++ show (length states - nrBurninCycles)
    hPutStr h $ unlines (headerLine:scoreLine:paramLines)
    return $ points !! minIndex
  where
   getOrderStats minI vals =
        let nPoints = fromIntegral $ length vals :: Double
            [lowCIindex, midIndex, highCIindex] = map (floor . (*nPoints)) [0.025, 0.5, 0.975]
            sortedVals = sort vals
        in  vals!!minI : map (sortedVals!!) [lowCIindex, midIndex, highCIindex]

reportTrace :: [String] -> [MCMCstate] -> Handle -> IO ()
reportTrace paramNames states h = do
    let body = do
            st <- states
            let l = [V.singleton (mcmcCurrentValue st), mcmcCurrentPoint st, mcmcStepWidths st,
                     mcmcSuccesRate st]
            return . intercalate "\t" . map show . V.toList . V.concat $ l
        headerLine = intercalate "\t" $ ["Score"] ++ paramNames ++ map (++"_delta") paramNames ++
                     map (++"_success") paramNames
    hPutStr h $ unlines (headerLine:body)
