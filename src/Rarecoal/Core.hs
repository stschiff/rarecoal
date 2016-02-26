module Rarecoal.Core (defaultTimes, getTimeSteps, getProb, validateModel, choose,
                      ModelEvent(..), EventType(..), ModelSpec(..), popJoinA,
                      popJoinB) where

import Rarecoal.StateSpace (JointStateSpace(..), makeJointStateSpace, getNonZeroStates)

import Control.Error.Safe (assertErr)
import Control.Exception.Base (assert)
import Control.Monad (when, forM_, forM, foldM)
import Control.Monad.ST (runST, ST)
import Data.List (sortBy, nub)
import Data.STRef (STRef, readSTRef, newSTRef, modifySTRef, writeSTRef)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Debug.Trace (trace)

data CoalState s = CoalState {
    _csA :: VM.MVector s Double,
    _csB :: VM.MVector s Double,
    _csBtemp :: VM.MVector s Double,
    _csNonZeroStates :: STRef s [Int],
    _csD :: STRef s Double,
    _csStateSpace :: JointStateSpace
}

data ModelEvent = ModelEvent {
    meTime :: Double,
    meEventType ::EventType
} deriving (Show, Read)

data EventType = Join Int Int
               | Split Int Int Double
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
    _msPopSize :: VM.MVector s Double,
    _msGrowthRates :: VM.MVector s Double,
    _msFreezeState :: VM.MVector s Bool,
    _msMigrationRates :: STRef s [(Int, Int, Double)]
}

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
            propagateStates ms cs timeSteps noShortcut
            readSTRef (_csD cs)
        combFac = product $ zipWith choose nVec config
        err = "Overflow Error in getProb for nVec=" ++ show nVec ++ ", kVec=" ++ show config
    assertErr err $ combFac > 0
    --trace (show $ combFac) $ return ()
    return $ d * mTheta modelSpec * combFac

makeInitModelState :: ModelSpec -> Int -> ST s (ModelState s)
makeInitModelState (ModelSpec _ _ events) nrPop = do
    sortedEvents <- newSTRef $ sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) ->
                                       time1 `compare` time2) events
    t <- newSTRef 0.0
    popSize <- VM.replicate nrPop 1.0
    growthRates <- VM.replicate nrPop 0.0
    freezeState <- VM.replicate nrPop False
    migrationMatrix <- newSTRef []
    return $ ModelState t sortedEvents popSize growthRates freezeState migrationMatrix

makeInitCoalState :: [Int] -> [Int] -> ST s (CoalState s)
makeInitCoalState nVec config = do
    a <- V.thaw . V.fromList $ zipWith (\n c -> fromIntegral $ n - c) nVec config
    let initialState = V.fromList config
        maxAf = sum config
        nrPop = length nVec
        jointStateSpace = makeJointStateSpace nrPop maxAf
        initialId = (_jsStateToId jointStateSpace) initialState
    b <- VM.replicate (_jsNrStates jointStateSpace) 0.0
    -- trace (show ("makeInitCoalState", _jsNrStates jointStateSpace, initialState, initialId)) $ 
    --        return ()
    VM.write b initialId 1.0
    nonZeroStates <- newSTRef (getNonZeroStates jointStateSpace [initialId])
    bTemp <- VM.new (_jsNrStates jointStateSpace)
    d <- newSTRef 0.0
    return $ CoalState a b bTemp nonZeroStates d jointStateSpace

propagateStates :: ModelState s -> CoalState s -> [Double] -> Bool -> ST s ()
propagateStates _ _ [] _ = return ()
propagateStates ms cs (nextTime:restTimes) noShortcut = do
    useShortcut <- canUseShortcut ms cs
    if (useShortcut && not noShortcut) then propagateStateShortcut ms cs else do
        singleStep ms cs nextTime
        propagateStates ms cs restTimes noShortcut

canUseShortcut :: ModelState s -> CoalState s -> ST s Bool
canUseShortcut ms cs = do
    nonZeroStates <- readSTRef (_csNonZeroStates cs)
    let idToState = _jsIdToState . _csStateSpace $ cs
        allDerivedHaveCoalesced =
            and [(V.length . V.filter (>0)) (idToState xId) == 1 | xId <- nonZeroStates]
    a <- V.freeze $ _csA cs
    let allAncestralHaveCoalesced = (V.length . V.filter (>0.0)) a == 1
    r <- V.freeze $ _msGrowthRates ms
    e <- readSTRef $ _msEventQueue ms
    return $ allDerivedHaveCoalesced && allAncestralHaveCoalesced && V.all (==0.0) r && null e
    
propagateStateShortcut :: ModelState s -> CoalState s -> ST s ()
propagateStateShortcut ms cs = do
    let stateSpace = _csStateSpace cs
        idToState = _jsIdToState stateSpace
    a <- V.freeze (_csA cs)
    let nrA = V.sum a
        popIndex = fst . head . filter ((>0.0) . snd) . zip [0..] . V.toList $ a
    popSize <- VM.read (_msPopSize ms) popIndex
    nonZeroIds <- readSTRef (_csNonZeroStates cs)
    let nonZeroStates = map idToState nonZeroIds
    probs <- mapM (VM.read (_csB cs)) nonZeroIds
    let additionalBranchLength =
            sum [prob * goState popSize nrA state | (state, prob) <- zip nonZeroStates probs]
    -- trace ("shortcut: " ++ show additionalBranchLength ++ "; " ++ show nrA ++ "; " ++
    --        show (zip nonZeroStates probs)) (return ())
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

singleStep :: ModelState s -> CoalState s -> Double -> ST s ()
singleStep ms cs nextTime = do
    -- debugOutput ms cs nextTime
    events <- readSTRef (_msEventQueue ms)
    let ModelEvent t _ = if null events then ModelEvent (1.0/0.0) undefined else head events
    if  t < nextTime then do
        performEvent ms cs
        singleStep ms cs nextTime
    else do
        deltaT <- (nextTime-) <$> readSTRef (_msT ms)
        when (deltaT > 0) $ do
            -- t <- use $ _1 . msT
            -- trace ("time: " ++ show t ++ ": stepping forward with deltaT=" ++
            --        show deltaT) $ return ()
            updateCoalState ms cs deltaT
            migrations <- readSTRef (_msMigrationRates ms)
            mapM_ (updateCoalStateMig ms cs deltaT) migrations
            updateModelState ms cs deltaT

debugOutput :: ModelState s -> CoalState s -> Double -> ST s ()
debugOutput ms cs nextTime = do
    currentTime <- readSTRef (_msT ms)
    aVec <- V.freeze (_csA cs)
    nonZeroStates <- readSTRef (_csNonZeroStates cs)
    bList <- mapM (VM.read (_csB cs)) nonZeroStates
    d <- readSTRef (_csD cs)
    trace (show nextTime ++ " " ++ show currentTime ++ " " ++ show aVec ++ " " ++
           show (zip nonZeroStates bList) ++ " " ++ show d) (return ())

performEvent :: ModelState s -> CoalState s -> ST s ()
performEvent ms cs = do
    events <- readSTRef (_msEventQueue ms)
    let ModelEvent _ e = head events
    -- t <- use $ _1 . msT
    -- trace ("time: " ++ show t ++ ": performing event " ++ show (head events)) $ return ()
    case e of
        Join k l -> popJoin ms cs k l
        Split k l m -> popSplit ms cs k l m
        SetPopSize k p -> do
            VM.write (_msPopSize ms) k p
            VM.write (_msGrowthRates ms) k 0.0
        SetGrowthRate k r -> VM.write (_msGrowthRates ms) k r
        SetFreeze k b -> VM.write (_msFreezeState ms) k b
        SetMigration k l m -> do
            migrations <- readSTRef (_msMigrationRates ms)
            let cleanedMigrations = [triple | triple@(k', l', m') <- migrations, k' /= k, l' /= l']
                newMigrations =
                    if m > 0.0 then (k, l, m) : cleanedMigrations else cleanedMigrations
            writeSTRef (_msMigrationRates ms) newMigrations
    writeSTRef (_msEventQueue ms) $ tail events

popJoin :: ModelState s -> CoalState s -> Int -> Int -> ST s ()
popJoin ms cs k l = do
    popJoinA (_csA cs) k l
    popJoinB (_csB cs) (_csBtemp cs) (_csNonZeroStates cs) (_csStateSpace cs) k l
    migrations <- readSTRef (_msMigrationRates ms)
    let newMigrations = deleteMigrations l migrations
    writeSTRef (_msMigrationRates ms) newMigrations
  where
    deleteMigrations pop list = [mig | mig@(k', l', _) <- list, k' /= pop, l' /= pop]

popJoinA :: VM.MVector s Double -> Int -> Int -> ST s ()
popJoinA aVec k l = do
    al <- VM.read aVec l
    ak <- VM.read aVec k
    VM.write aVec l 0.0
    VM.write aVec k (al + ak)

popJoinB :: VM.MVector s Double -> VM.MVector s Double -> STRef s [Int] -> JointStateSpace ->
            Int -> Int -> ST s ()
popJoinB bVec bVecTemp nonZeroStateRef stateSpace k l = do
    VM.set bVecTemp 0.0
    nonZeroStateIds <- readSTRef nonZeroStateRef
    newNonZeroStateIds <- forM nonZeroStateIds $ \xId -> do
        oldProb <- VM.read bVec xId
        let xVec = (_jsIdToState stateSpace) xId
            xl = xVec V.! l
            xk = xVec V.! k
            xVec' = xVec V.// [(k, xk + xl), (l, 0)]
            xId' = (_jsStateToId stateSpace) xVec'
        val <- VM.read bVecTemp xId'
        VM.write bVecTemp xId' (val + oldProb)
        return xId'
    VM.copy bVec bVecTemp
    writeSTRef nonZeroStateRef $ getNonZeroStates stateSpace (nub newNonZeroStateIds)

popSplit :: ModelState s -> CoalState s -> Int -> Int -> Double -> ST s ()
popSplit ms cs k l m = do
    popSplitA (_csA cs) k l m
    popSplitB (_csB cs) (_csBtemp cs) (_csNonZeroStates cs) (_csStateSpace cs) k l m 

popSplitA :: VM.MVector s Double -> Int -> Int -> Double -> ST s ()  
popSplitA aVec k l m = do
    al <- VM.read aVec l
    ak <- VM.read aVec k
    VM.write aVec l $ (1.0 - m) * al
    VM.write aVec k $ (m * al + ak)

popSplitB :: VM.MVector s Double -> VM.MVector s Double -> STRef s [Int] -> JointStateSpace ->
             Int -> Int -> Double -> ST s ()
popSplitB bVec bVecTemp nonZeroStateRef stateSpace k l m = do
    VM.set bVecTemp 0.0
    nonZeroStateIds <- readSTRef nonZeroStateRef
    newNonZeroStateIds <- fmap concat . forM nonZeroStateIds $ \xId -> do
        oldProb <- VM.read bVec xId
        let xVec = (_jsIdToState stateSpace) xId
            xl = xVec V.! l
            xk = xVec V.! k
        forM [0..xl] $ \s -> do
            let xVec' = xVec V.// [(k, xk + s), (l, xl - s)]
                xId' = (_jsStateToId stateSpace) xVec'
            val <- VM.read bVecTemp xId'
            let binomialFactor = m ^ s * (1.0 - m) ^ (xl - s) * choose xl s
            VM.write bVecTemp xId' (oldProb + val * binomialFactor)
            return xId'
    VM.copy bVec bVecTemp
    writeSTRef nonZeroStateRef $ getNonZeroStates stateSpace (nub newNonZeroStateIds)

updateCoalState :: ModelState s -> CoalState s -> Double -> ST s ()
updateCoalState ms cs deltaT = do
    updateA ms cs deltaT
    updateB ms cs deltaT
    updateD cs deltaT

updateA :: ModelState s -> CoalState s -> Double -> ST s ()
updateA ms cs deltaT = do
    let nrPop = _jsNrPop (_csStateSpace cs)
    forM_ [0 .. nrPop - 1] $ \k -> do
        popSizeK <- VM.read (_msPopSize ms) k
        freezeStateK <- VM.read (_msFreezeState ms) k
        aK <- VM.read (_csA cs) k
        let newA = if freezeStateK || aK < 1.0 then aK else propagateA deltaT popSizeK aK
        VM.write (_csA cs) k newA

propagateA :: Double -> Double -> Double -> Double
propagateA deltaT popSize a0 = 1.0 / (1.0 + ((1.0 / a0) - 1.0) * exp (- 0.5 * deltaT / popSize))

updateB :: ModelState s -> CoalState s -> Double -> ST s ()
updateB ms cs deltaT = do
    nonZeroStateIds <- readSTRef (_csNonZeroStates cs)
    let stateSpace = _csStateSpace cs
        nrPop = _jsNrPop stateSpace
    VM.set (_csBtemp cs) 0.0
    forM_ nonZeroStateIds $ \xId -> do
        let x1ups = (_jsX1up stateSpace) xId
            x = (_jsIdToState stateSpace) xId
            -- explicit definitions to introduce automatic CAFs for profiling
            f1 = foldM (t1TermHelper x (_msPopSize ms) (_csA cs) (_msFreezeState ms)) 0
                       [0..(nrPop - 1)]
            f2 = foldM (t2TermHelper (_csB cs) x1ups x (_msPopSize ms) (_msFreezeState ms)) 0 
                       [0..(nrPop - 1)]
        t1 <- f1
        t2 <- f2
        val <- VM.read (_csB cs) xId
        let newVal = val * exp (-t1 * deltaT) + t2
        VM.write (_csBtemp cs) xId newVal
    forM_ nonZeroStateIds $ \xId -> do
        valTemp <- VM.read (_csBtemp cs) xId
        VM.write (_csB cs) xId valTemp
  where
    t1TermHelper state popSize aVec freezeState val i = do
        let xx = fromIntegral (state V.! i)
        pp <- VM.read popSize i
        aa <- VM.read aVec i
        ff <- VM.read freezeState i
        let add = if ff then 0.0 else xx * (xx - 1.0) / 2.0 * (1.0 / pp) + xx * aa * (1.0 / pp)
        return $ val + add
    t2TermHelper bVec x1ups state popSize freezeState val i = do
        let index = x1ups V.! i
        bb <- if index == -1 then return 0.0 else VM.read bVec index
        let xx = fromIntegral (state V.! i)
        pp <- VM.read popSize i
        ff <- VM.read freezeState i
        let add = if ff then 0.0 else
                bb * (1.0 - exp (-xx * (xx + 1.0) / 2.0 * (1.0 / pp) * deltaT))
        return $ val + add

updateD :: CoalState s -> Double -> ST s ()
updateD cs deltaT = do
    let stateSpace = _csStateSpace cs
        nrPop = _jsNrPop stateSpace
        x1s = map (_jsX1 stateSpace) [0 .. (nrPop - 1)]
    b1s <- mapM (VM.read (_csB cs)) x1s
    modifySTRef (_csD cs) (\v -> v + deltaT * sum b1s)

updateCoalStateMig :: ModelState s -> CoalState s -> Double -> (Int, Int, Double) -> ST s ()
updateCoalStateMig ms cs deltaT (k, l, m) = popSplit ms cs k l (deltaT * m)

updateModelState :: ModelState s -> CoalState s -> Double -> ST s ()
updateModelState ms cs deltaT = do
    let nrPop = _jsNrPop (_csStateSpace cs)
    forM_ [0 .. (nrPop - 1)] $ \k -> do
        popSize <- VM.read (_msPopSize ms) k
        growthRate <- VM.read (_msGrowthRates ms) k
        let newPopSize = popSize * exp (-(growthRate * deltaT))
        VM.write (_msPopSize ms) k newPopSize
    modifySTRef (_msT ms) (+deltaT)

validateModel :: ModelSpec -> Either String ()
validateModel (ModelSpec _ _ events) = do
    when (or [t < 0 | ModelEvent t _ <- events]) $ Left "Negative event times"
    let sortedEvents =
            sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2) events
    checkEvents sortedEvents
  where
    checkEvents [] = Right ()
    checkEvents (ModelEvent _ (Join k l):rest) =
        if k == l || or [k' == l || l' == l | ModelEvent _ (Join k' l') <- rest]
            then Left "Illegal joins"
            else checkEvents rest
    checkEvents (ModelEvent _ (SetPopSize _ p):rest) =
        if p < 0.001 || p > 1000 then Left $ "Illegal population size: " ++ show p else
            checkEvents rest
    checkEvents (ModelEvent _ (SetGrowthRate _ r):rest) =
        if abs r > 10000.0 then Left "Illegal growth rates" else checkEvents rest
    checkEvents (ModelEvent _ (SetMigration _ _ r):rest) =
        if r < 0.0 || r > 10000.0 then Left "Illegal migration rate" else checkEvents rest
    checkEvents (ModelEvent _ (Split _ _ m):rest) =
        if m < 0.0 || m > 1.0 then Left "Illegal split rate" else checkEvents rest
    checkEvents (ModelEvent _ (SetFreeze _ _):rest) = checkEvents rest

choose :: Int -> Int -> Double
choose _ 0 = 1
choose n k = product [fromIntegral (n + 1 - j) / fromIntegral j | j <- [1..k]]

-- see https://en.wikipedia.org/wiki/Binomial_coefficient
chooseCont :: Double -> Int -> Double
chooseCont _ 0 = 1
chooseCont n k = product [(n + 1.0 - fromIntegral j) / fromIntegral j | j <- [1..k]]
