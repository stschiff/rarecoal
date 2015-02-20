{-# LANGUAGE TemplateHaskell #-}

module Core (defaultTimes, getProb, validateModel, ModelEvent(..), EventType(..), ModelSpec(..)) where

import Control.Monad.Trans.State.Lazy (State, get, put, execState)
import Data.List (sortBy)
import Debug.Trace (trace)
import Utils (computeAllConfigs)
import Control.Monad (when, foldM, forM_)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import Data.Vector.Unboxed.Base (Unbox)
import qualified Data.Map as M
import Control.Error.Safe (assertErr)
import Control.Lens ((%~), ix, (&), makeLenses, use, (%=), uses, (+~), (-~), (*=), (+=), _1, _2, (.=))

(!) :: Unbox a => V.Vector a -> Int -> a
(!) = (V.!)
(//) :: Unbox a => V.Vector a -> [(Int, a)] -> V.Vector a
(//) = (V.//)

type JointState = V.Vector Int

data CoalState = CoalState {
    _csA :: V.Vector Double,
    _csB :: M.Map JointState Double,
    _csD :: Double,
    _csMaxMVec :: JointState,
    _csX1up :: M.Map JointState [JointState],
    _csX1 :: [JointState]
} deriving (Show)

makeLenses ''CoalState

data ModelEvent = ModelEvent {
    meTime :: Double,
    meEventType ::EventType
} deriving (Show, Read)

data EventType = Join Int Int
               | SetPopSize Int Double
               | SetGrowthRate Int Double 
               | SetFreeze Int Bool
               | SetMigration Int Int Double
               deriving (Show, Read)

data ModelSpec = ModelSpec {
    mTimeSteps :: [Double],
    mTheta :: Double,
    mEvents :: [ModelEvent]
} deriving (Show)

data ModelState = ModelState {
    _msT :: Double,
    _msEventQueue :: [ModelEvent],
    _msPopSize :: V.Vector Double,
    _msGrowthRates :: V.Vector Double,
    _msFreezeState :: V.Vector Bool,
    _msMigrationRates :: [(Int, Int, Double)]
} deriving (Show)

makeLenses ''ModelState

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
    return $ _csD fcs * mTheta modelSpec * combFac

makeInitModelState :: ModelSpec -> Int -> ModelState
makeInitModelState (ModelSpec _ _ events) k =
    let sortedEvents = sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2) events
        t = 0.0
        popSize = V.replicate k 1.0
        growthRates = V.replicate k 0.0
        freezeState = V.replicate k False
        migrationMatrix = []
    in  ModelState t sortedEvents popSize growthRates freezeState migrationMatrix

makeInitCoalState :: [Int] -> [Int] -> CoalState
makeInitCoalState nVec config =
    let a = V.fromList $ zipWith (\n m -> fromIntegral (n - m)) nVec config
        initialState = V.fromList config
        b = fillStateSpace initialState $ M.singleton initialState 1.0
        nrPop = V.length initialState
        x1 = [V.replicate nrPop 0 // [(k, 1)] | k <- [0..nrPop-1]]
    in  CoalState a b 0.0 initialState M.empty x1

fillStateSpace :: JointState -> M.Map JointState Double -> M.Map JointState Double
fillStateSpace maxMVec b =
    let allStates = expandPattern maxMVec
        safeInsert m k = M.insertWith (\_ oldVal -> oldVal) k 0.0 m
    in  foldl safeInsert b allStates
  where
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
    -- trace (show nextTime ++ " " ++ show (_msT ms) ++ " " ++ show (_csA cs) ++ " " ++ show (_csB cs)) (return ())
    let events = _msEventQueue ms
        ModelEvent t _ = if null events then ModelEvent (1.0/0.0) undefined else head events
    if  t < nextTime then do
        singleStep t
        performEvent
        singleStep nextTime
    else do
        let deltaT = nextTime - _msT ms
        updateCoalState deltaT
        migrations <- use $ _1 . msMigrationRates
        mapM_ (updateCoalStateMig deltaT) migrations
        updateModelState deltaT

performEvent :: State (ModelState, CoalState) ()
performEvent = do
    events <- use $ _1 . msEventQueue
    let ModelEvent t e = head events
    case e of
        Join k l -> popJoin k l
        SetPopSize k p -> do
            _1 . msPopSize . ix k .= p
            _1 . msGrowthRates . ix k .= 0.0
        SetGrowthRate k r -> _1 . msGrowthRates . ix k .= r
        SetFreeze k b -> _1 . msFreezeState . ix k .= b
        SetMigration k l m -> do
            migrations <- use $ _1 . msMigrationRates
            let migList = [i | ((k', l', _), i) <- zip migrations [0..], k' == k, l' == l]
            when (length migList == 0 && m > 0) $ _1 . msMigrationRates %= (++[(k, l, m)])
            when (length migList > 0 && m == 0) $ _1 . msMigrationRates %= deleteFromList (head migList)
    _1 . msEventQueue .= tail events
  where
    deleteFromList index l = [v | (v, i) <- zip l [0..], i /= index]

popJoin :: Int -> Int -> State (ModelState, CoalState) ()
popJoin k l = do
    a <- use $ _2 . csA
    _2 . csA . ix l .= 0
    _2 . csA . ix k += a!l
    _2 . csB %= M.mapKeysWith (+) (joinCounts k l)
    _2 . csMaxMVec %= joinCounts k l
    maxMVec <- use $ _2 . csMaxMVec
    _2 . csB %= fillStateSpace maxMVec
    _2 . csB %= M.filterWithKey (\k _ -> k!l == 0)
    _1 . msMigrationRates %= deleteMigrations l
  where
    deleteMigrations pop list = [mig | mig@(k', l', _) <- list, k' /= pop, l' /= pop]

joinCounts :: Int -> Int -> JointState -> JointState
joinCounts k l s = 
   let newK = s!k + s!l
       newL = 0
   in  s // [(k, newK), (l, newL)]

updateCoalState :: Double -> State (ModelState, CoalState) ()
updateCoalState deltaT = do
    updateA deltaT
    updateB deltaT
    updateD deltaT

updateCoalStateMig :: Double -> (Int, Int, Double) -> State (ModelState, CoalState) ()
updateCoalStateMig deltaT (k, l, m) = do
    b <- use $ _2 . csB
    let sourceStates = filter (\vec -> vec!l > 0) $ M.keys b
    _2 . csB .= foldl updateState b sourceStates
    a <- use $ _2 . csA
    _2 . csA . ix l *= exp (-deltaT * m)
    _2 . csA . ix k += a!l * (1.0 - exp (-deltaT * m))
  where
    updateState :: M.Map JointState Double -> JointState -> M.Map JointState Double
    updateState b sourceState =
        let bProb = b M.! sourceState
            reduceFactor = exp $ -deltaT * m * fromIntegral (sourceState!l)
            b' = M.adjust (*reduceFactor) sourceState b
            targetState = sourceState & ix k +~ 1 & ix l -~ 1
        in  if targetState `M.member` b' then
                M.insertWith (+) targetState (bProb * (1.0 - reduceFactor)) b'
            else
                M.insert targetState (bProb * (1.0 - reduceFactor)) . fillStateSpace targetState $ b'

updateA :: Double -> State (ModelState, CoalState) ()
updateA deltaT = do
    popSize <- use $ _1 . msPopSize
    freezeState <- use $ _1 . msFreezeState
    _2 . csA %= V.zipWith3 go popSize freezeState
  where
    go popSizeK freezeStateK aK = aK * if freezeStateK then 1.0 else
        exp (-0.5 * (aK - 1.0) * (1.0 / popSizeK) * deltaT)

updateB :: Double -> State (ModelState, CoalState) ()
updateB deltaT = do
    popSize <- use $ _1 . msPopSize
    freezeState <- use $ _1 . msFreezeState
    allStates <- uses (_2 . csB) M.keys 
    b <- use $ _2 . csB
    a <- use $ _2 . csA
    forM_ allStates $ \x -> do
        let nrPop = V.length x
        x1ups <- lookupX1up x
        let b1ups = V.fromList [M.findWithDefault 0.0 x1up b | x1up <- x1ups]
            x' = V.map fromIntegral x
            t1s = [x'!k * (x'!k - 1) / 2.0 * (1.0 / popSize!k) + x'!k * a!k * (1.0 / popSize!k) |
                   k <- [0..nrPop-1], not $ freezeState!k]
            t2s = [b1ups!l * (1.0 - exp (-x'!l * (x'!l + 1) / 2.0 * (1.0 / popSize!l) * deltaT)) |
                   l <- [0..nrPop-1], not $ freezeState!l]
            val = b M.! x
        _2 . csB %= M.insert x (val * exp (-(sum t1s) * deltaT) + sum t2s)
  where
    lookupX1up :: JointState -> State (ModelState, CoalState) [JointState]
    lookupX1up x = do
        x1upRet <- uses (_2 . csX1up) $ M.lookup x
        case x1upRet of
            Just x1up -> return x1up
            Nothing -> do
                let x1up = [x // [(k, x!k + 1)] | k <- [0..(V.length x)-1]]
                _2 . csX1up %= M.insert x x1up
                return x1up
        

updateD :: Double -> State (ModelState, CoalState) ()
updateD deltaT = do
    x1s <- use $ _2 . csX1
    b <- use $ _2 . csB
    let b1s = [M.findWithDefault 0.0 x1 b | x1 <- x1s]
    _2 . csD += deltaT * sum b1s

updateModelState :: Double -> State (ModelState, CoalState) ()
updateModelState deltaT = do
    (ms, cs) <- get
    let popSize = _msPopSize ms
        t = _msT ms
        growthRates = _msGrowthRates ms
        popSize' = [(popSize!k) * exp (-(growthRates!k) * deltaT) | k <- [0..(V.length popSize)-1]]
    put (ms {_msT = t + deltaT, _msPopSize = V.fromList popSize'}, cs)

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
    checkEvents (ModelEvent _ (SetMigration _ _ r):rest) =
        if abs r < 0.0 then Left "Illegal migration rate" else checkEvents rest

choose :: Int -> Int -> Double
choose _ 0 = 1
choose n k = product [(fromIntegral $ n + 1 - j) / fromIntegral j | j <- [1..k]]

--chooseLog :: Int -> Int -> Double
--chooseLog _ 0 = 0
--chooseLog n k = sum [(log . fromIntegral $ n + 1 - j) - log $ fromIntegral j | j <- [1..k]]
