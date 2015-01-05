module Core (defaultTimes, getProb, update, validateModel, ModelEvent(..), EventType(..), ModelSpec(..)) where

import Control.Monad.Trans.State.Lazy (State, get, put, execState)
import Data.List (sortBy, elemIndex, nub)
import Debug.Trace (trace)
import Utils (computeAllConfigs)
import Control.Monad (when)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import Data.Int (Int64)
import Control.Error.Safe (assertErr)

(!) = (V.!)
(//) = (V.//)

type JointState = V.Vector Int

data CoalState = CoalState {
    csA :: V.Vector Double,
    csB :: V.Vector Double,
    csStateList :: VB.Vector JointState,
    csD :: Double
} deriving (Show)

data ModelEvent = ModelEvent {
    meTime :: Double,
    meEventType ::EventType
} deriving (Show, Read)

data EventType = Join Int Int | SetPopSize Int Double | SetGrowthRate Int Double deriving (Show, Read)

data ModelSpec = ModelSpec {
    mTimeSteps :: [Double],
    mTheta :: Double,
    mEvents :: [ModelEvent]
} deriving (Show)

data ModelState = ModelState {
    msT :: Double,
    msEventQueue :: [ModelEvent],
    msPopSize :: V.Vector Double,
    msGrowthRates :: V.Vector Double
} deriving (Show)

defaultTimes :: [Double]
defaultTimes = getTimeSteps 20000 400 20.0

getTimeSteps :: Int -> Int -> Double -> [Double]
getTimeSteps n0 lingen tMax =
    let tMin     = 1.0 / (2.0 * fromIntegral n0)
        alpha    = fromIntegral lingen / (2.0 * fromIntegral n0)
        nr_steps = floor $ logBase (1.0 + tMin / alpha) (1.0 + tMax / alpha)
    in  map (getTimeStep alpha nr_steps) [1..nr_steps-1]
  where
    getTimeStep :: Double -> Int -> Int -> Double
    getTimeStep alpha nr_steps i =
        alpha * exp (fromIntegral i / fromIntegral nr_steps * log (1.0 + tMax / alpha)) - alpha

getProb :: ModelSpec -> [Int] -> [Int] -> Either String Double
getProb modelSpec nVec config = do
    validateModel modelSpec
    let timeSteps = mTimeSteps modelSpec
        ims = makeInitModelState modelSpec (length nVec)
        ics = makeInitCoalState nVec config
        (_, fcs) = execState (mapM_ singleStep timeSteps) (ims, ics)
        combFac = product $ zipWith choose nVec config
        err = "Overflow Error in getProb for nVec=" ++ show nVec ++ ", kVec=" ++ show config
    assertErr err $ combFac > 0
    --trace (show $ combFac) $ return ()
    return $ csD fcs * mTheta modelSpec * combFac

makeInitModelState :: ModelSpec -> Int -> ModelState
makeInitModelState (ModelSpec _ _ events) k =
    let sortedEvents = sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2) events
        t = 0.0
        popSize = V.replicate k 1.0
        growthRates = V.replicate k 0.0
    in  ModelState t sortedEvents popSize growthRates

makeInitCoalState :: [Int] -> [Int] -> CoalState
makeInitCoalState nVec config =
    let a = V.fromList $ zipWith (\n m -> fromIntegral (n - m)) nVec config
        nrPop = length nVec
        stateList = makeStandardStateList config
        initialState = V.fromList config
        Just initialStateIndex = initialState `V.elemIndex` stateList
        b = V.replicate (length stateList) 0.0 // [(initialStateIndex, 1.0)]
    in  CoalState a b stateList 0.0

makeStandardStateList :: [Int] -> VB.Vector JointState
makeStandardStateList maxMvec = 
    let nrPop = length maxMvec
        allStates = map V.fromList $ computeAllConfigs nrPop (maximum maxMvec)
    in  VB.fromList (filter (isBelowMax maxMvec) . filter (noZeros maxMvec) $ allStates)
  where
    isBelowMax maxMvec state = and [s <= m | (s, m) <- zip (V.toList state) maxMvec]
    noZeros maxMvec state = and [s > 0 || m == 0 | (s, m) <- zip (V.toList state) maxMvec]

singleStep :: Double -> State (ModelState, CoalState) ()
singleStep nextTime = do
    (ms, cs) <- get
    --when (nextTime < 0.0002) $ trace (show nextTime ++ " " ++ show (msT ms) ++ " " ++ show (csA cs) ++ " " ++ show (csB cs)) (return ())
    --trace (show (msT ms) ++ " " ++ show (V.toList $ csA cs) ++ " " ++ show (map V.toList $ csB cs) ++ show (csD cs)) (return ())
    let events = msEventQueue ms
        ModelEvent t _ = if null events then ModelEvent (1.0/0.0) undefined else head events
    if  t < nextTime then do
        singleStep t
        performEvent
        singleStep nextTime
    else do
        let deltaT = nextTime - msT ms
        updateCoalState deltaT
        updateModelState deltaT

performEvent :: State (ModelState, CoalState) ()
performEvent = do
    (ModelState _ events popSize growthRates, cs) <- get
    let ModelEvent t' e = head events
    case e of
        Join k l -> do
            let cs' = popJoin k l cs
            put (ModelState t' (tail events) popSize growthRates, cs')
        SetPopSize k p -> do
            let popSize' = update k p popSize
                growthRates' = update k 0.0 growthRates
            put (ModelState t' (tail events) popSize' growthRates', cs)
        SetGrowthRate k r -> do
            let growthRates' = update k r growthRates
            put (ModelState t' (tail events) popSize growthRates', cs)

popJoin :: Int -> Int -> CoalState -> CoalState
popJoin k l cs =
    let aVec = csA cs
        bVec = csB cs
        stateList = csStateList cs
        newAk = aVec!k + aVec!l
        newA  = aVec // [(k, newAk), (l, 0.0)]
        newStateList = VB.fromList $ nub [joinCounts k l s | s <- VB.toList stateList]
        newB = joinProbs k l stateList newStateList bVec
    in  CoalState newA newB newStateList (csD cs)

joinCounts :: Int -> Int -> JointState -> JointState
joinCounts k l s = 
   let newK = s!k + s!l
       newL = 0
   in  s // [(k, newK), (l, newL)]

joinProbs :: Int -> Int -> VB.Vector JointState -> VB.Vector JointState -> V.Vector Double -> V.Vector Double
joinProbs k l oldStateList newStateList oldB =
    let init = V.replicate (length newStateList) 0.0
    in  go (VB.toList oldStateList) (V.toList oldB) init
  where
    go :: [JointState] -> [Double] -> V.Vector Double -> V.Vector Double
    go [] _ b = b
    go (state:states) (bprob:bprobs) b = 
        let newState = joinCounts k l state
            Just newStateIndex = newState `elemIndex` newStateList
            newBprob = b!newStateIndex + bprob
            newB = b // [(newStateIndex, newBprob)]
        in  go states bprobs newB
    
update :: Int -> a -> [a] -> [a]
update i val vec = let (x, _:ys) = splitAt i vec in x ++ (val:ys)

updateCoalState :: Double -> State (ModelState, CoalState) ()
updateCoalState deltaT = do
    (ms, cs) <- get
    let popSize = msPopSize ms
        aNew = updateA deltaT popSize (csA cs)
        bNew = updateB deltaT popSize (csA cs) (csB cs) (csStateList cs)
        dNew = updateD deltaT (csB cs) (csD cs)
    put (ms, CoalState aNew bNew (csStateList cs) dNew)

updateA :: Double -> V.Vector Double -> V.Vector Double -> V.Vector Double
updateA deltaT popSize = V.zipWith go popSize
  where
    go popSizeK aK = aK * approxExp (-0.5 * (aK - 1.0) * (1.0 / popSizeK) * deltaT)

approxExp :: Double -> Double
approxExp = exp --if abs arg < 0.05 then 1.0 + arg else exp arg

updateB :: Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> VB.Vector JointState -> V.Vector Double
updateB deltaT popSize a b stateList =
    V.generate (V.length b) go
  where
    go stateIndex =
        let state = stateList!stateIndex
            nrPop = V.length state
            x = state!stateIndex
            x1ups = [x // [(k, x!k + 1)] | k <- [0..nrPop]]
            x1upIndices = [x1up `VB.elemIndex` stateList | x1up <- x1ups]
            b1up = V.fromList [maybe 0.0 (b!) x1upIndex | x1upIndex <- x1upIndices]
            t1 = sum [x!k * (x!k - 1) / 2.0 * (1.0 / popSize!k) + x!k * a!k * (1.0 / popSize!k) | k <- [0..nrPop]]
            t2 = sum [b1up!l * (1.0 - exp (x!l * (x!l + 1) / 2.0 * (1.0 / popSize!l) * deltaT)) | l <- [0..nrPop]]
        in  b!stateIndex * exp (-t1 * deltaT) + t2

updateD :: Double -> V.Vector Double -> VB.Vector JointState -> Double -> Double
updateD deltaT b stateList d =
    let nrPop = V.length (stateList!1)
        x1s = [V.replicate nrPop 0 // [(k, 1)] | k <- [0..nrPop]]
        x1Indices = [x1 `V.elemIndex` stateList | x1 <- x1s]
        b1s = V.fromList [maybe 0.0 (b!) x1Index | x1Index <- x1Indices]
    in d + deltaT * sum b1s

updateModelState :: Double -> State (ModelState, CoalState) ()
updateModelState deltaT = do
    (ms, cs) <- get
    let popSize = msPopSize ms
        t = msT ms
        growthRates = msGrowthRates ms
        popSize' = V.fromList [popSize!k * exp (-(growthRates!k) * deltaT) | k <- [0..(V.length popSize)]]
    put (ms {msT = t + deltaT, msPopSize = popSize'}, cs)

validateModel :: ModelSpec -> Either String ()
validateModel (ModelSpec _ _ events) = do
    when (or [t < 0 | ModelEvent t _ <- events]) $ Left "Negative event times"
    let sortedEvents = sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2) events
    checkEvents sortedEvents
  where
    checkEvents [] = Right ()
    checkEvents (ModelEvent _ (Join k l):rest) =
        if k == l || or [k' == l || l' == l | ModelEvent _ (Join k' l') <- rest]
            then Left "Illegal joins"
            else checkEvents rest
    checkEvents (ModelEvent _ (SetPopSize _ p):rest) =
        if p < 0.001 || p > 1000 then Left $ "Illegal populaton size: " ++ show p else checkEvents rest
    checkEvents (ModelEvent _ (SetGrowthRate _ r):rest) =
        if abs r  > 1000.0 then Left "Illegal growth rates" else checkEvents rest

choose :: Int -> Int -> Double
choose _ 0 = 1
choose n k = product [(fromIntegral $ n + 1 - j) / fromIntegral j | j <- [1..k]]

--chooseLog :: Int -> Int -> Double
--chooseLog _ 0 = 0
--chooseLog n k = sum [(log . fromIntegral $ n + 1 - j) - log $ fromIntegral j | j <- [1..k]]
