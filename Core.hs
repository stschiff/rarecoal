module Core (defaultTimes, getProb, update, validateModel, ModelEvent(..), EventType(..), ModelSpec(..)) where

import Control.Monad.Trans.State.Lazy (State, get, put, execState)
import Data.List (sortBy)
import Debug.Trace (trace)
import Utils (computeAllConfigs)
import Control.Monad (when, foldM)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Base (Unbox)
import qualified Data.Map as M
import Control.Error.Safe (assertErr)

(!) :: Unbox a => V.Vector a -> Int -> a
(!) = (V.!)
(//) :: Unbox a => V.Vector a -> [(Int, a)] -> V.Vector a
(//) = (V.//)

type JointState = V.Vector Int

data CoalState = CoalState {
    csA :: V.Vector Double,
    csB :: M.Map JointState Double,
    csD :: Double,
    csMaxMVec :: JointState,
    csX1up :: M.Map JointState [JointState],
    csX1 :: [JointState]
} deriving (Show)

data ModelEvent = ModelEvent {
    meTime :: Double,
    meEventType ::EventType
} deriving (Show, Read)

data EventType = Join Int Int
               | SetPopSize Int Double
               | SetGrowthRate Int Double 
               | SetFreeze Int Bool
               deriving (Show, Read)

data ModelSpec = ModelSpec {
    mTimeSteps :: [Double],
    mTheta :: Double,
    mEvents :: [ModelEvent]
} deriving (Show)

data ModelState = ModelState {
    msT :: Double,
    msEventQueue :: [ModelEvent],
    msPopSize :: V.Vector Double,
    msGrowthRates :: V.Vector Double,
    msFreezeState :: V.Vector Bool
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
        freezeState = V.replicate k False
    in  ModelState t sortedEvents popSize growthRates freezeState

makeInitCoalState :: [Int] -> [Int] -> CoalState
makeInitCoalState nVec config =
    let a = V.fromList $ zipWith (\n m -> fromIntegral (n - m)) nVec config
        initialState = V.fromList config
        b = M.singleton initialState 1.0
        b' = fillStateSpace b initialState
        x1upMap = M.mapWithKey (\x _ -> x1ups x) b'
        nrPop = V.length initialState
        x1 = [V.replicate nrPop 0 // [(k, 1)] | k <- [0..nrPop-1]]
    in  CoalState a b' 0.0 initialState x1upMap x1

x1ups :: JointState -> [JointState]
x1ups x = [x // [(k, x!k + 1)] | k <- [0..nrPop-1]]
  where
    nrPop = V.length x

fillStateSpace :: M.Map JointState Double -> JointState -> M.Map JointState Double
fillStateSpace b maxMVec =
    let allStates = expandPattern maxMVec
        safeInsert m k = M.insertWith (\_ oldVal -> oldVal) k 0.0 m
    in  foldl safeInsert b allStates

expandPattern :: JointState -> [JointState]
expandPattern vec =
    let k = V.length vec
    in  foldM go vec [0..k-1]
  where    
    go vec_ i =
        let maxVal = vec_ ! i
        in if maxVal <= 1 then [vec_] else [vec_ // [(i, val)] | val <- [1..maxVal]]

singleStep :: Double -> State (ModelState, CoalState) ()
singleStep nextTime = do
    (ms, cs) <- get
    -- trace (show nextTime ++ " " ++ show (msT ms) ++ " " ++ show (csA cs) ++ " " ++ show (csB cs)) (return ())
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
    (ModelState _ events popSize growthRates freezeState, cs) <- get
    let ModelEvent t e = head events
    case e of
        Join k l -> do
            let cs' = popJoin k l cs
            put (ModelState t (tail events) popSize growthRates freezeState, cs')
        SetPopSize k p -> do
            let popSize' = popSize // [(k, p)]
                growthRates' = growthRates // [(k, 0.0)]
            put (ModelState t (tail events) popSize' growthRates' freezeState, cs)
        SetGrowthRate k r -> do
            let growthRates' = growthRates // [(k, r)]
            put (ModelState t (tail events) popSize growthRates' freezeState, cs)
        SetFreeze k b -> do
            let freezeState' = freezeState // [(k, b)]
            put (ModelState t (tail events) popSize growthRates freezeState', cs)

popJoin :: Int -> Int -> CoalState -> CoalState
popJoin k l cs =
    let a = csA cs
        b = csB cs
        maxMvec = csMaxMVec cs
        newAk = a!k + a!l
        newA  = a // [(k, newAk), (l, 0.0)]
        newB = M.mapKeysWith (+) (joinCounts k l) b
        newMaxMvec = joinCounts k l maxMvec
        newB' = fillStateSpace newB newMaxMvec
        newX1upMap = M.mapWithKey (\x _ -> x1ups x) newB'
    in  CoalState newA newB' (csD cs) newMaxMvec newX1upMap (csX1 cs)

joinCounts :: Int -> Int -> JointState -> JointState
joinCounts k l s = 
   let newK = s!k + s!l
       newL = 0
   in  s // [(k, newK), (l, newL)]

update :: Int -> a -> [a] -> [a]
update i val vec = let (x, _:ys) = splitAt i vec in x ++ (val:ys)

updateCoalState :: Double -> State (ModelState, CoalState) ()
updateCoalState deltaT = do
    (ms, cs) <- get
    let popSize = msPopSize ms
        freezeState = msFreezeState ms
        aNew = updateA deltaT popSize freezeState (csA cs)
        bNew = updateB deltaT popSize freezeState (csA cs) (csB cs) (csX1up cs)
        dNew = updateD deltaT (csB cs) (csD cs) (csX1 cs)
    put (ms, cs {csA = aNew, csB = bNew, csD = dNew})

updateA :: Double -> V.Vector Double -> V.Vector Bool -> V.Vector Double -> V.Vector Double
updateA deltaT popSize freezeState = V.zipWith3 go popSize freezeState
  where
    go popSizeK freezeStateK aK = aK * if freezeStateK then 1.0 else
        approxExp (-0.5 * (aK - 1.0) * (1.0 / popSizeK) * deltaT)

approxExp :: Double -> Double
approxExp = exp --if abs arg < 0.05 then 1.0 + arg else exp arg

updateB :: Double -> V.Vector Double -> V.Vector Bool -> V.Vector Double -> M.Map JointState Double
                  -> M.Map JointState [JointState] -> M.Map JointState Double
updateB deltaT popSize freezeState a b x1upMap =
    M.mapWithKey go b    
  where
    go :: JointState -> Double -> Double
    go x val =
        let nrPop = V.length x
            x1ups = x1upMap M.! x
            b1ups = V.fromList [M.findWithDefault 0.0 x1up b | x1up <- x1ups]
            x' = V.map fromIntegral x
            t1s = [x'!k * (x'!k - 1) / 2.0 * (1.0 / popSize!k) + x'!k * a!k * (1.0 / popSize!k) |
                   k <- [0..nrPop-1], not $ freezeState!k]
            t2s = [b1ups!l * (1.0 - exp (-x'!l * (x'!l + 1) / 2.0 * (1.0 / popSize!l) * deltaT)) |
                   l <- [0..nrPop-1], not $ freezeState!l]
        in  val * exp (-(sum t1s) * deltaT) + sum t2s

updateD :: Double -> M.Map JointState Double -> Double -> [JointState] -> Double
updateD deltaT b d x1s =
    let nrPop = V.length $ head (M.keys b)
        b1s = [M.findWithDefault 0.0 x1 b | x1 <- x1s]
    in  d + deltaT * sum b1s

updateModelState :: Double -> State (ModelState, CoalState) ()
updateModelState deltaT = do
    (ms, cs) <- get
    let popSize = msPopSize ms
        t = msT ms
        growthRates = msGrowthRates ms
        popSize' = [(popSize!k) * exp (-(growthRates!k) * deltaT) | k <- [0..(V.length popSize)-1]]
    put (ms {msT = t + deltaT, msPopSize = V.fromList popSize'}, cs)

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
        if abs r > 1000.0 then Left "Illegal growth rates" else checkEvents rest

choose :: Int -> Int -> Double
choose _ 0 = 1
choose n k = product [(fromIntegral $ n + 1 - j) / fromIntegral j | j <- [1..k]]

--chooseLog :: Int -> Int -> Double
--chooseLog _ 0 = 0
--chooseLog n k = sum [(log . fromIntegral $ n + 1 - j) - log $ fromIntegral j | j <- [1..k]]
