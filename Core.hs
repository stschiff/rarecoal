module Core (defaultTimes, getProb, update, validateModel, ModelEvent(..), EventType(..), ModelSpec(..)) where

import Control.Monad.Trans.State.Lazy (State, get, put, execState)
import Data.List (sortBy)
import Debug.Trace (trace)
import Control.Monad (when)
import qualified Data.Vector.Unboxed as V
import Data.Int (Int64)
import Control.Error.Safe (assertErr)

(!) :: V.Vector Double -> Int -> Double
(!) = (V.!)
(//) :: V.Vector Double -> [(Int, Double)] -> V.Vector Double
(//) = (V.//)

data CoalState = CoalState {
    csA :: V.Vector Double,
    csB :: [V.Vector Double],
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
    msPopSize :: [Double],
    msGrowthRates :: [Double]
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
        popSize = replicate k 1.0
        growthRates = replicate k 0.0
    in  ModelState t sortedEvents popSize growthRates

makeInitCoalState :: [Int] -> [Int] -> CoalState
makeInitCoalState nVec config =
    let a = V.fromList $ zipWith (\n m -> fromIntegral (n - m)) nVec config
        b = [V.generate (m + 1) (\i -> if i == m then 1.0 else 0.0) | m <- config]
    in  CoalState a b 0.0

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
        new_aK = aVec!k + aVec!l
        new_a  = aVec // [(k, new_aK), (l, 0.0)]
        bk = bVec !! k
        bl = bVec !! l
        new_m = V.length bk + V.length bl - 2
        new_b_k = V.generate (new_m + 1) (joinProbs bk bl)
        new_b_l = V.singleton 1.0
        new_b = update k new_b_k bVec
        new_b' = update l new_b_l new_b
    in  CoalState new_a new_b' (csD cs)

joinProbs :: V.Vector Double -> V.Vector Double -> Int -> Double
joinProbs bVec1 bVec2 i =
    let m1 = V.length bVec1 - 1
        m2 = V.length bVec2 - 1
    in  sum [bVec1!j * bVec2!(i - j) | j <- [0..i], j <= m1 && (i-j) <= m2]

update :: Int -> a -> [a] -> [a]
update i val vec = let (x, _:ys) = splitAt i vec in x ++ (val:ys)

updateCoalState :: Double -> State (ModelState, CoalState) ()
updateCoalState deltaT = do
    (ms, cs) <- get
    let popSize = msPopSize ms
        aNew = updateA deltaT popSize (csA cs)
        bNew = updateB deltaT popSize (csA cs) (csB cs)
        dNew = updateD deltaT (csB cs) (csD cs)
    put (ms, CoalState aNew bNew dNew)

updateA :: Double -> [Double] -> V.Vector Double -> V.Vector Double
updateA deltaT popSize = V.zipWith go (V.fromList popSize)
  where
    go popSizeK aK = aK * approxExp(-0.5 * (aK - 1.0) * (1.0 / popSizeK) * deltaT)

approxExp :: Double -> Double
approxExp = exp --if abs arg < 0.05 then 1.0 + arg else exp arg

updateB :: Double -> [Double] -> V.Vector Double -> [V.Vector Double] -> [V.Vector Double]
updateB deltaT popSize a = zipWith3 (updateBk deltaT) popSize (V.toList a)

updateBk :: Double -> Double -> Double -> V.Vector Double -> V.Vector Double
updateBk deltaT popSizeK aK bK =
    let m = (V.length bK - 1)
    in  V.generate (m + 1) go
  where
    go i =
        let bKi = bK!i
            bKi' = if i < (V.length bK - 1) then bK!(i + 1) else 0.0
            i' = fromIntegral i
        in  bKi * approxExp(-(i' * (i' - 1) / 2.0 * (1.0 / popSizeK) + i' * aK * (1.0 / popSizeK)) * deltaT)
            + bKi' * (1.0 - approxExp(-(i' * (i' + 1)) / 2.0 * (1.0 / popSizeK) * deltaT))

updateD :: Double -> [V.Vector Double] -> Double -> Double
updateD deltaT b d = d + deltaT * sum (map go [0..(kn - 1)])
  where
    kn   = length b
    go k = if V.length (b!!k) == 1 then 0.0 else product $ zipWith (\l bl -> bl!(if l /= k then 0 else 1)) [0..] b

updateModelState :: Double -> State (ModelState, CoalState) ()
updateModelState deltaT = do
    (ms, cs) <- get
    let popSize = msPopSize ms
        t = msT ms
        growthRates = msGrowthRates ms
        popSize' = zipWith (\p r -> p * exp(-r * deltaT)) popSize growthRates
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
