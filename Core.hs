module Core (ModelEvent(..), EventType(..), ModelSpec(..), defaultTimes, getProb) where

import Math.Combinatorics.Exact.Binomial (choose)
import Control.Monad.State (State, get, put, execState)
import Data.List (sortBy)
import Debug.Trace (trace)
import qualified Data.Vector.Unboxed as V

(!) = (V.!)
(//) = (V.//)

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

data CoalState = CoalState {
    csA :: V.Vector Double,
    csB :: [V.Vector Double],
    csD :: Double
} deriving (Show)

data ModelState = ModelState {
    msT :: Double,
    msEventQueue :: [ModelEvent],
    msPopSize :: [Double],
    msGrowthRates :: [Double]
} deriving (Show)

defaultTimes = getTimeSteps 20000 400 20.0

getTimeSteps :: Int -> Int -> Double -> [Double]
getTimeSteps n0 lingen tMax =
    let tMin     = 1.0 / (2.0 * fromIntegral n0)
        alpha    = fromIntegral lingen / (2.0 * fromIntegral n0)
        nr_steps = floor $ log(1.0 + tMax / alpha) / log(1.0 + tMin / alpha)
    in  map (getTimeStep alpha nr_steps tMax) [1..nr_steps-1]
  where
    getTimeStep alpha nr_steps tMax i =
        alpha * exp (fromIntegral i / fromIntegral nr_steps * log (1.0 + tMax / alpha)) - alpha

getProb :: ModelSpec -> [Int] -> [Int] -> Either String Double
getProb modelSpec nVec config = do
    let timeSteps = mTimeSteps modelSpec
        ims = makeInitModelState modelSpec (length nVec)
        ics = makeInitCoalState nVec config
    checkJoins [e | ModelEvent t e@(Join _ _) <- msEventQueue ims]
    let (fms, fcs) = execState (mapM_ singleStep timeSteps) (ims, ics)
    return $ csD fcs * (mTheta modelSpec) * fromIntegral (product $ zipWith choose nVec config)
  where
    checkJoins [] = Right ()
    checkJoins (Join k l:rest) =
        if k == l || any (\(Join k' l') -> k' == l || l' == l) rest
            then Left "Illegal joins"
            else checkJoins rest
 
makeInitModelState :: ModelSpec -> Int -> ModelState
makeInitModelState (ModelSpec timeSteps theta events) k =
    let sortedEvents = sortBy (\(ModelEvent time1 type1) (ModelEvent time2 type2) -> time1 `compare` time2) events
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
    let events = msEventQueue ms
        ModelEvent t e = if null events then ModelEvent (1.0/0.0) undefined else head $ events
    if  t < nextTime then do
        singleStep t
        performEvent
        singleStep nextTime
    else do
        let deltaT = nextTime - msT ms
        updateCoalState deltaT
        updateModelState deltaT
    -- if nextTime < 0.0002 then trace (show nextTime ++ " " ++ show cs ()(put ms) else put ms

performEvent :: State (ModelState, CoalState) ()
performEvent = do
    (ModelState t events popSize growthRates, cs) <- get
    let ModelEvent t' e = head events
    case e of
        Join k l -> do
            let cs' = popJoin k l cs
            put (ModelState t' (tail events) popSize growthRates, cs')
        SetPopSize k p -> do
            let popSize' = update k p popSize
                growthRates' = replicate (length growthRates) 0.0
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
update i val vec = let (x, y:ys) = splitAt i vec in x ++ (val:ys)

updateCoalState :: Double -> State (ModelState, CoalState) ()
updateCoalState deltaT = do
    (ms, cs) <- get
    let popSize = msPopSize ms
        aNew = updateA deltaT popSize (csA cs)
        bNew = updateB deltaT popSize (csA cs) (csB cs)
        dNew = updateD deltaT (csB cs) (csD cs)
    put (ms, CoalState aNew bNew dNew)

updateA :: Double -> [Double] -> V.Vector Double -> V.Vector Double
updateA deltaT popSize a = V.zipWith go (V.fromList popSize) a
  where
    go popSizeK aK = aK * approx_exp(-0.5 * (aK - 1.0) * (1.0 / popSizeK) * deltaT)

approx_exp :: Double -> Double
approx_exp arg = exp arg --if abs arg < 0.05 then 1.0 + arg else exp arg

updateB :: Double -> [Double] -> V.Vector Double -> [V.Vector Double] -> [V.Vector Double]
updateB deltaT popSize a b = zipWith3 (updateBk deltaT) popSize (V.toList a) b

updateBk :: Double -> Double -> Double -> V.Vector Double -> V.Vector Double
updateBk deltaT popSizeK aK bK =
    let m = (V.length bK - 1)
    in  V.generate (m + 1) go
  where
    go i =
        let bKi = bK!i
            bKi' = if i < (V.length bK - 1) then bK!(i + 1) else 0.0
            i' = fromIntegral i
        in  bKi * approx_exp(-(i' * (i' - 1) / 2.0 * (1.0 / popSizeK) + i' * aK * (1.0 / popSizeK)) * deltaT)
            + bKi' * (1.0 - approx_exp(-(i' * (i' + 1)) / 2.0 * (1.0 / popSizeK) * deltaT))

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


