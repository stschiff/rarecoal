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
import qualified Data.IntMap as M
import Control.Error.Safe (assertErr)
import Control.Lens ((%~), ix, (&), makeLenses, use, (%=), uses, (+~),
                     (-~), (*=), (+=), _1, _2, _3, (.=), (^.))
import Data.MemoCombinators (arrayRange)
import Control.Exception.Base (assert)


(!) :: Unbox a => V.Vector a -> Int -> a
(!) = (V.!)
(//) :: Unbox a => V.Vector a -> [(Int, a)] -> V.Vector a
(//) = (V.//)

type JointState = V.Vector Int

data JointStateSpace = JointStateSpace  {
    _jsStateToId :: JointState -> Int,
    _jsIdToState :: Int -> JointState,
    _jsX1up :: Int -> [Int],
    _jsX1 :: Int -> Int,
    _jsNrPop :: Int,
    _jsMaxAf :: Int
}

makeLenses ''JointStateSpace

data CoalState = CoalState {
    _csA :: V.Vector Double,
    _csB :: M.IntMap Double,
    _csD :: Double,
    _csStateSpace :: JointStateSpace
}

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
        (_, fcs) = execState (propagateStates timeSteps) (ims, ics)
        combFac = product $ zipWith choose nVec config
        err = "Overflow Error in getProb for nVec=" ++ show nVec ++ ", kVec=" ++ show config
    assertErr err $ combFac > 0
    --trace (show $ combFac) $ return ()
    return $ _csD fcs * mTheta modelSpec * combFac

makeInitModelState :: ModelSpec -> Int -> ModelState
makeInitModelState (ModelSpec _ _ events) nrPop =
    let sortedEvents = sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2) events
        t = 0.0
        popSize = V.replicate nrPop 1.0
        growthRates = V.replicate nrPop 0.0
        freezeState = V.replicate nrPop False
        migrationMatrix = []
    in  ModelState t sortedEvents popSize growthRates freezeState migrationMatrix

makeInitCoalState :: [Int] -> [Int] -> CoalState
makeInitCoalState nVec config =
    let a = V.fromList $ zipWith (\n c -> fromIntegral $ n - c) nVec config
        initialState = V.fromList config
        jointStateSpace = makeJointStateSpace initialState
        initialId = (jointStateSpace ^. jsStateToId) initialState
        b = fillStateSpace jointStateSpace $ M.singleton initialId 1.0
    in  CoalState a b 0.0 jointStateSpace

makeJointStateSpace :: JointState -> JointStateSpace
makeJointStateSpace config =
    let maxAf = V.sum config
        stateToId = genericStateToId (maxAf + 1)
        nrPop = V.length config
        idToState = genericIdToState (maxAf + 1) nrPop
        x1up = map stateToId . genericX1Up . idToState
        x1 = stateToId . genericX1 nrPop
        maxId = genericMaxId (maxAf + 1) nrPop
        idToStateMemo = arrayRange (0, maxId - 1) idToState
        x1upMemo = arrayRange (0, maxId - 1) x1up
        x1Memo = arrayRange (0, maxId - 1) x1
    in  JointStateSpace stateToId idToStateMemo x1upMemo x1Memo nrPop maxAf
    
genericStateToId :: Int -> JointState -> Int
genericStateToId base state = if V.any (>=base) state then -1 else V.ifoldl (\v i x -> v + x * base ^ i) 0 state

genericMaxId :: Int -> Int -> Int
genericMaxId base nrPop = base ^ nrPop

genericIdToState :: Int -> Int -> Int -> JointState
genericIdToState base nrPop id_ = V.fromList $ take nrPop (go id_)
  where
    go x = x `mod` base : go (x `div` base)

genericX1Up :: JointState -> [JointState]
genericX1Up x = [x // [(k, x!k + 1)] | k <- [0..V.length x - 1]]

genericX1 :: Int -> Int -> JointState
genericX1 n k = V.replicate n 0 // [(k, 1)]

fillStateSpace :: JointStateSpace -> M.IntMap Double -> M.IntMap Double
fillStateSpace jointStateSpace b =
    let states = [jointStateSpace ^. jsIdToState $ key | (key, prob) <- M.toList b, prob > 0.0]
        nrPop = jointStateSpace ^. jsNrPop
        maxAf = jointStateSpace ^. jsMaxAf
        maxMVec = V.fromList . map maximum $ [map (V.!i) states | i <- [0 .. nrPop - 1]]
        allStates = expandPattern maxMVec
        allStateIds = map (jointStateSpace ^. jsStateToId) allStates
        safeInsert m k = M.insertWith (\_ oldVal -> oldVal) k 0.0 m
    in  foldl safeInsert b allStateIds

expandPattern :: JointState -> [JointState]
expandPattern maxMVec =
    let k = V.length maxMVec
        maxAf = V.sum maxMVec
    in  filter (\v -> V.sum v > 0 && V.sum v <= maxAf) $ foldM go maxMVec [0..k-1]
  where
    go vec_ i = 
        let maxVal = vec_ ! i
        in if maxVal == 0 then [vec_] else [vec_ // [(i, val)] | val <- [0..maxVal]]

propagateStates :: [Double] -> State (ModelState, CoalState) ()
propagateStates [] = return ()
propagateStates (nextTime:restTimes) = do
    useShortcut <- canUseShortcut
    if useShortcut then propagateStateShortcut else do
        singleStep nextTime
        propagateStates restTimes

canUseShortcut :: State (ModelState, CoalState) Bool
canUseShortcut = do
    (ms, cs) <- get
    let a = _csA cs
        b = _csB cs
        js = _csStateSpace cs
        nonZeroStates = [(_jsIdToState js) xId | (xId, prob) <- M.toList b, prob > 0.0]
        allDerivedHaveCoalesced = and [(V.length . V.filter (>0)) x == 1 | x <- nonZeroStates]
        allAncestralHaveCoalesced = (V.length . V.filter (>0.0)) a == 1
        r = _msGrowthRates ms
        e = _msEventQueue ms
    return $ allDerivedHaveCoalesced && allAncestralHaveCoalesced && V.all (==0.0) r && null e

propagateStateShortcut :: State (ModelState, CoalState) ()
propagateStateShortcut = do
    nrA <- uses (_2 . csA) (round . V.sum)
    b <- use $ _2 . csB
    idToState <- use $ _2 . csStateSpace . jsIdToState
    let additionalBranchLength = sum [prob * goState nrA (idToState xId) | (xId, prob) <- M.toList b, prob > 0.0]
    _2 . csB %= M.map (const 0)
    _2 . csD += additionalBranchLength
    _2 . csA %= V.map (const 1)
  where
    goState nrA x =
        let nrDerived = assert ((V.length . V.filter (>0)) x == 1) $ V.sum x
        in  singlePopMutBranchLength nrA nrDerived
        
singlePopMutBranchLength :: Int -> Int -> Double
singlePopMutBranchLength nrA nrDerived =
    let withCombinatorics = 2.0 / fromIntegral nrDerived
        combFactor = choose (nrA + nrDerived) nrDerived
    in  withCombinatorics / combFactor

singleStep :: Double -> State (ModelState, CoalState) ()
singleStep nextTime = do
    -- (ms, cs) <- get
    -- trace (show nextTime ++ " " ++ show (_msT ms) ++ " " ++ show (_csA cs) ++ " " ++ show (_csB cs)) (return ())
    events <- use $ _1 . msEventQueue
    let ModelEvent t _ = if null events then ModelEvent (1.0/0.0) undefined else head events
    if  t < nextTime then do
        singleStep t
        performEvent
        singleStep nextTime
    else do
        deltaT <- uses (_1 . msT) (nextTime-)
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
            when (null migList && m > 0) $ _1 . msMigrationRates %= (++[(k, l, m)])
            when (not (null migList)) $ _1 . msMigrationRates . ix (head migList) . _3 .= m
            _1 . msMigrationRates %= filter (\(_,_,m) -> m > 0.0)
    _1 . msEventQueue .= tail events
  where
    deleteFromList index l = [v | (v, i) <- zip l [0..], i /= index]

popJoin :: Int -> Int -> State (ModelState, CoalState) ()
popJoin k l = do
    a <- use $ _2 . csA
    _2 . csA . ix l .= 0
    _2 . csA . ix k += a!l
    stateSpace <- use $ _2 . csStateSpace
    _2 . csB %= M.mapKeysWith (+) (joinCounts stateSpace k l)
    _2 . csB %= fillStateSpace stateSpace
    let idToState = stateSpace ^. jsIdToState
    _2 . csB %= M.filterWithKey (\k _ -> (idToState k)!l == 0)
    _1 . msMigrationRates %= deleteMigrations l
  where
    deleteMigrations pop list = [mig | mig@(k', l', _) <- list, k' /= pop, l' /= pop]

joinCounts :: JointStateSpace -> Int -> Int -> Int -> Int
joinCounts stateSpace k l id_ = 
    let s = (stateSpace ^. jsIdToState) id_
        newK = s!k + s!l
        newS = s // [(k, newK), (l, 0)]
    in  (stateSpace ^. jsStateToId) newS

updateCoalState :: Double -> State (ModelState, CoalState) ()
updateCoalState deltaT = do
    updateA deltaT
    updateB deltaT
    updateD deltaT

updateCoalStateMig :: Double -> (Int, Int, Double) -> State (ModelState, CoalState) ()
updateCoalStateMig deltaT (k, l, m) = do
    b <- use $ _2 . csB
    stateSpace <- use $ _2 . csStateSpace
    _2 . csB .= foldl (updateState stateSpace) b (M.keys b)
    a <- use $ _2 . csA
    _2 . csA . ix l *= exp (-deltaT * m)
    _2 . csA . ix k += a!l * (1.0 - exp (-deltaT * m))
  where
    updateState :: JointStateSpace -> M.IntMap Double -> Int -> M.IntMap Double
    updateState stateSpace b sourceId =
        let bProb = b M.! sourceId
            sourceState = (stateSpace ^. jsIdToState) sourceId
        in  if sourceState ! l > 0 then
                let reduceFactor = exp $ -deltaT * m * fromIntegral (sourceState!l)
                    b' = M.adjust (*reduceFactor) sourceId b
                    targetState = sourceState & ix k +~ 1 & ix l -~ 1
                    targetId = (stateSpace ^. jsStateToId) targetState
                in  if targetId `M.member` b' then
                        M.insertWith (+) targetId (bProb * (1.0 - reduceFactor)) b'
                    else
                        fillStateSpace stateSpace . M.insert targetId (bProb * (1.0 - reduceFactor)) $ b'
            else b
            

updateA :: Double -> State (ModelState, CoalState) ()
updateA deltaT = do
    popSize <- use $ _1 . msPopSize
    freezeState <- use $ _1 . msFreezeState
    -- aBefore <- use $ _2 . csA
    _2 . csA %= V.zipWith3 go popSize freezeState
    -- aAfter <- use $ _2 . csA
    -- trace (show aBefore ++ " " ++ show aAfter) $ return ()
  where
    go popSizeK freezeStateK aK = aK * if freezeStateK || aK < 1.0 then 1.0 else
        exp (-0.5 * (aK - 1.0) * (1.0 / popSizeK) * deltaT)

updateB :: Double -> State (ModelState, CoalState) ()
updateB deltaT = do
    popSize <- use $ _1 . msPopSize
    freezeState <- use $ _1 . msFreezeState
    allIds <- uses (_2 . csB) M.keys 
    a <- use $ _2 . csA
    stateSpace <- use $ _2 . csStateSpace
    forM_ allIds $ \xId -> do
        b <- use $ _2 . csB
        let x1ups = stateSpace ^. jsX1up $ xId
            b1ups = V.fromList [M.findWithDefault 0.0 x1up b | x1up <- x1ups]
            x = stateSpace ^. jsIdToState $ xId
            nrPop = V.length x
            x' = V.map fromIntegral x
            t1s = V.zipWith4 t1func x' popSize a freezeState
            t2s = V.zipWith4 t2func b1ups x' popSize freezeState 
            val = b M.! xId
            newVal = val * exp (-(V.sum t1s) * deltaT) + V.sum t2s
        _2 . csB %= M.insert xId newVal
  where
    t1func xx pp aa ff = if ff then 0.0 else xx * (xx - 1.0) / 2.0 * (1.0 / pp) + xx * aa * (1.0 / pp)
    t2func bb xx pp ff = if ff then 0.0 else bb * (1.0 - exp (-xx * (xx + 1.0) / 2.0 * (1.0 / pp) * deltaT))

updateD :: Double -> State (ModelState, CoalState) ()
updateD deltaT = do
    b <- use $ _2 . csB
    x1 <- use $ _2 . csStateSpace . jsX1
    nrPop <- use $ _2 . csStateSpace . jsNrPop
    let b1s = [M.findWithDefault 0.0 (x1 k) b | k <- [0 .. nrPop - 1]]
    _2 . csD += deltaT * sum b1s

updateModelState :: Double -> State (ModelState, CoalState) ()
updateModelState deltaT = do
    (ms, cs) <- get
    let popSizes = _msPopSize ms
        t = _msT ms
        growthRates = _msGrowthRates ms
        nrPop = V.length popSizes
        popSizes' = V.fromList [(popSizes!k) * exp (-(growthRates!k) * deltaT) | k <- [0..nrPop - 1]]
    -- trace (show (_msT ms) ++ " " ++ show popSizes ++ " " ++ show popSizes') $
    put (ms {_msT = t + deltaT, _msPopSize = popSizes'}, cs)

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
        if r < 0.0 then Left "Illegal migration rate" else checkEvents rest

choose :: Int -> Int -> Double
choose _ 0 = 1
choose n k = product [fromIntegral (n + 1 - j) / fromIntegral j | j <- [1..k]]

