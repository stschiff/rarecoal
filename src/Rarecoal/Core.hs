{-# LANGUAGE TemplateHaskell #-}

module Rarecoal.Core (defaultTimes, getTimeSteps, getProb, validateModel,
                      ModelEvent(..), EventType(..), ModelSpec(..)) where

import Control.Error.Safe (assertErr)
import Control.Exception.Base (assert)
import Control.Lens (ix, (&), makeLenses, use, (%=), uses, (+~),
                     (-~), (*=), (+=), _1, _2, _3, (.=), (^.), view)
import Control.Monad (when, foldM, forM_)
import Control.Monad.ST (runST)
import Control.Monad.Trans.State.Lazy (State, get, put, execState)
import Data.List (sortBy)
import Data.Foldable (foldl')
import qualified Data.IntMap as M
import Data.MemoCombinators (arrayRange)
import Data.STRef (STRef, readSTRef, newSTRef, modifySTRef)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Vector.Unboxed.Base (Unbox)

type JointState = V.Vector Int

data JointStateSpace = JointStateSpace  {
    _jsStateToId :: JointState -> Int,
    _jsIdToState :: Int -> JointState,
    _jsX1up :: Int -> [Int],
    _jsX1 :: Int -> Int,
    _jsNrPop :: Int,
    _jsMaxAf :: Int,
    _jsMaxId :: Int
}

makeLenses ''JointStateSpace

data CoalState s = CoalState {
    _csA :: VM.MVector s Double,
    _csAtemp :: VM.MVector s Double,
    _csB :: VM.Vector s Double,
    _csBtemp :: VM.Vector s Double,
    _csNonZeroStates :: STRef s [Int]
    _csD :: STRef s Double,
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

data ModelState s = ModelState {
    _msT :: STRef s Double,
    _msEventQueue :: STRef s [ModelEvent],
    _msPopSize :: VM.Vector s Double,
    _msGrowthRates :: VM.Vector s Double,
    _msFreezeState :: VM.Vector s Bool,
    _msMigrationRates :: STRef s [(Int, Int, Double)]
}

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

getProb :: ModelSpec -> [Int] -> Bool -> [Int] -> Either String Double
getProb modelSpec nVec noShortcut config = do
    validateModel modelSpec
    let timeSteps = mTimeSteps modelSpec
        d = runST $ do
            ms <- makeInitModelState modelSpec (length nVec)
            cs <- makeInitCoalState nVec config
            propagateStates timeSteps noShortcut ms cs
            readSTRef (_csD cs)
        combFac = product $ zipWith choose nVec config
        err = "Overflow Error in getProb for nVec=" ++ show nVec ++ ", kVec=" ++ show config
    assertErr err $ combFac > 0
    --trace (show $ combFac) $ return ()
    return $ d * mTheta modelSpec * combFac

makeInitModelState :: ModelSpec -> Int -> ST s ModelState
makeInitModelState (ModelSpec _ _ events) nrPop = do
    let sortedEvents = sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2) events
    t <- newSTRef 0.0
    popSize <- VM.replicate nrPop 1.0
    growthRates <- VM.replicate nrPop 0.0
    freezeState <- VM.replicate nrPop False
    migrationMatrix <- newSTRef []
    return $ ModelState t sortedEvents popSize growthRates freezeState migrationMatrix

makeInitCoalState :: [Int] -> [Int] -> ST s CoalState
makeInitCoalState nVec config = do
    a <- V.thaw . V.fromList $ zipWith (\n c -> fromIntegral $ n - c) nVec config
    aTemp <- VM.new (length nVec)
    let initialState = V.fromList config
        maxAf = sum config
        nrPop = length nVec
        jointStateSpace = makeJointStateSpace nrPop maxAf
        initialId = (jointStateSpace ^. jsStateToId) initialState
    b <- VM.replicate (_jsMaxId jointStateSpace) 0.0
    VM.write b initialId 1.0
    nonZeroStates <- newSTRef [initialId]
    bTemp <- VM.new (_jsMaxId jointStateSpace)
    d <- newSTRef 0.0
    return $ CoalState a aTemp b bTemp nonZeroStates d jointStateSpace

makeJointStateSpace :: Int -> Int -> JointStateSpace
makeJointStateSpace nrPop maxAf =
    let stateToId = genericStateToId (maxAf + 1)
        idToState = genericIdToState (maxAf + 1) nrPop
        x1up = map stateToId . genericX1Up . idToState
        x1 = stateToId . genericX1 nrPop
        maxId = genericMaxId (maxAf + 1) nrPop
        idToStateMemo = arrayRange (0, maxId - 1) idToState
        x1upMemo = arrayRange (0, maxId - 1) x1up
        x1Memo = arrayRange (0, maxId - 1) x1
    in  JointStateSpace stateToId idToStateMemo x1upMemo x1Memo nrPop maxAf maxId

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

propagateStates :: [Double] -> Bool -> ModelState -> CoalState -> ST s ()
propagateStates [] _ _ _ = return ()
propagateStates (nextTime:restTimes) noShortcut ms cs = do
    useShortcut <- canUseShortcut ms cs
    if (useShortcut && not noShortcut) then propagateStateShortcut else do
        singleStep nextTime
        propagateStates restTimes noShortcut

canUseShortcut :: ModelState -> CoalState -> ST s Bool
canUseShortcut ms cs = do
    let nrPop = view (csStateSpace . jsNrPop) cs
    nonZeroStates <- readSTRef (_csNonZeroStates cs)
    let idToState = _jsIdToState . _csStateSpace $ cs
        allDerivedHaveCoalesced = and [(V.length . V.filter (>0)) (idToState xId) == 1 | xId <- nonZeroStates]
    a <- V.freeze $ _csA cs
    let allAncestralHaveCoalesced = (V.length . V.filter (>0.0)) a == 1
    r <- V.freeze $ _msGrowthRates ms
    e <- readSTRef $ _msEventQueue ms
    return $ allDerivedHaveCoalesced && allAncestralHaveCoalesced && V.all (==0.0) r && null e
    
propagateStateShortcut :: ModelState -> CoalState -> ST s ()
propagateStateShortcut ms cs = do
    let nrPop = view (csStateSpace . jsNrPop) cs
        idToState = view (csStateSpace . jsIdToState) cs
    a <- V.freeze (_csA cs)
    let nrA = V.sum a
        popIndex = fst . head . filter ((>0.0) . snd) . zip [0..] $ a
    popSize <- VM.read (_msPopSize ms) popIndex
    nonZeroIds <- readSTRef (_csNonZeroStates cs)
    let nonZeroStates = map idToState nonZeroIds
    probs <- mapM (VM.read (_csB cs)) nonZeroIds
    let additionalBranchLength =
            sum [prob * goState popSize nrA state | (state, prob) <- zip nonZeroStates probs]
    VM.set (_csB cs) 0.0
    modifySTRef (_csD cs) (+additionalBranchLength)
    VM.write (_csA cs) popIndex 1.0
  where
    goState popSize nrA x =
        let nrDerived = assert ((V.length . V.filter (>0)) x == 1) $ V.sum x
        in  singlePopMutBranchLength popSize nrA nrDerived

singlePopMutBranchLength :: Double -> Double -> Int -> Double
singlePopMutBranchLength popSize nrA nrDerived =
    let withCombinatorics = 2.0 * popSize / fromIntegral nrDerived
        combFactor = chooseCont (nrA + fromIntegral nrDerived) nrDerived
    in  withCombinatorics / combFactor

singleStep :: ModelState -> CoalState -> Double -> ST s ()
singleStep ms cs nextTime = do
    -- trace (show nextTime ++ " " ++ show (_msT ms) ++ " " ++ show (_csA cs) ++ " " ++ show (_csB cs)) (return ())
    events <- readSTRef (_msEventQueue ms)
    let ModelEvent t _ = if null events then ModelEvent (1.0/0.0) undefined else head events
    if  t < nextTime then do
        -- singleStep t
        performEvent ms cs
        singleStep nextTime
    else do
        deltaT <- (nextTime-) <$> readSTRef (_msT ms)
        when (deltaT > 0) $ do
            -- t <- use $ _1 . msT
            -- trace ("time: " ++ show t ++ ": stepping forward with deltaT=" ++ show deltaT) $ return ()
            updateCoalState ms cs deltaT
            migrations <- readSTRef (_msMigrationRates ms)
            mapM_ (updateCoalStateMig ms cs deltaT) migrations
            updateModelState ms cs deltaT

performEvent :: ModelState -> CoalState -> ST s ()
performEvent ms cs = do
    events <- readSTRef (_msEventQueue ms)
    let ModelEvent _ e = head events
    -- t <- use $ _1 . msT
    -- trace ("time: " ++ show t ++ ": performing event " ++ show (head events)) $ return ()
    case e of
        Join k l -> popJoin ms cs k l
        SetPopSize k p -> do
            VM.write (_msPopSize ms) k p
            VM.write (_msGrowthRates ms) k 0.0
        SetGrowthRate k r -> VM.write (_msGrowthRates ms) k r
        SetFreeze k b -> VM.write (_msFreezeState ms) k b
        SetMigration k l m -> do
            migrations <- readSTRef (_msMigrationRates ms)
            let migList = [i | ((k', l', _), i) <- zip migrations [0..], k' == k, l' == l]
            let newMigList =
                if (null migList && m > 0) then
                    (k, l, m) : migList
                else
                    if not (null migList) then migList . (ix (head migList) . _3) .= m
                    else migList
            writeSTRef (_msMigrationRates ms) $ filter (\(_,_,m') -> m' > 0.0) newMigList
    writeSTRef (_msEventQueue ms) $ tail events

-- below here still needs to be done
popJoin :: Int -> Int -> State (ModelState, CoalState) ()
popJoin k l = do
    a <- use $ _2 . csA
    _2 . csA . ix l .= 0
    _2 . csA . ix k += a!l
    stateSpace <- use $ _2 . csStateSpace
    _2 . csB %= M.mapKeysWith (+) (joinCounts stateSpace k l)
    _2 . csB %= fillStateSpace stateSpace
    let idToState = stateSpace ^. jsIdToState
    _2 . csB %= M.filterWithKey (\k' _ -> (idToState k')!l == 0)
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
    freeze <- use $ _1 . msFreezeState
    if freeze!k || freeze!l then return () else do
        b <- use $ _2 . csB
        stateSpace <- use $ _2 . csStateSpace
        _2 . csB .= foldl' (updateState stateSpace) b (M.keys b)
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
    _2 . csA %= V.zipWith3 go popSize freezeState
  where
    go popSizeK freezeStateK aK = if freezeStateK || aK < 1.0 then aK else propagateA deltaT popSizeK aK

propagateA :: Double -> Double -> Double -> Double
propagateA deltaT popSize a0 = 1.0 / (1.0 + ((1.0 / a0) - 1.0) * exp (- 0.5 * deltaT / popSize))

updateB :: Double -> State (ModelState, CoalState) ()
updateB deltaT = do
    popSize <- use $ _1 . msPopSize
    freezeState <- use $ _1 . msFreezeState
    allIds <- uses (_2 . csB) M.keys
    a <- use $ _2 . csA
    -- let midPointA = V.zipWith (propagateA (deltaT / 2.0)) popSize a
    stateSpace <- use $ _2 . csStateSpace
    forM_ allIds $ \xId -> do
        b <- use $ _2 . csB
        let x1ups = stateSpace ^. jsX1up $ xId
            b1ups = V.fromList [M.findWithDefault 0.0 x1up b | x1up <- x1ups]
            x = stateSpace ^. jsIdToState $ xId
            x' = V.map fromIntegral x
            -- t1s = V.zipWith4 t1func x' popSize midPointA freezeState
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
        if p < 0.001 || p > 1000 then Left $ "Illegal population size: " ++ show p else checkEvents rest
    checkEvents (ModelEvent _ (SetGrowthRate _ r):rest) =
        if abs r > 10000.0 then Left "Illegal growth rates" else checkEvents rest
    checkEvents (ModelEvent _ (SetMigration _ _ r):rest) =
        if r < 0.0 || r > 10000.0 then Left "Illegal migration rate" else checkEvents rest
    checkEvents (ModelEvent _ (SetFreeze _ _):rest) = checkEvents rest

choose :: Int -> Int -> Double
choose _ 0 = 1
choose n k = product [fromIntegral (n + 1 - j) / fromIntegral j | j <- [1..k]]

-- see https://en.wikipedia.org/wiki/Binomial_coefficient
chooseCont :: Double -> Int -> Double
chooseCont _ 0 = 1
chooseCont n k = product [(n + 1.0 - fromIntegral j) / fromIntegral j | j <- [1..k]]
