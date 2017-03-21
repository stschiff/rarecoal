module Rarecoal.Core (defaultTimes, getTimeSteps, getProb, validateModel,
    choose, ModelEvent(..), EventType(..), ModelSpec(..), popJoinA,
    popJoinB, popSplitA, popSplitB, getNrOfPops, getRegularizationPenalty) where

import           Rarecoal.StateSpace         (JointStateSpace (..),
                                              getNonZeroStates,
                                              makeJointStateSpace)

import           Control.Error.Safe          (assertErr)
import           Control.Exception.Base      (assert)
import           Control.Monad               (filterM, foldM, forM, forM_, when,
                                              (>=>))
import           Control.Monad.ST            (ST, runST)
import           Data.List                   (nub, sortBy)
import           Data.STRef                  (STRef, modifySTRef, newSTRef,
                                              readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
-- import Debug.Trace (trace)

data CoalState s = CoalState {
    _csA             :: VM.MVector s Double,
    _csB             :: VM.MVector s Double,
    _csBtemp         :: VM.MVector s Double,
    _csNonZeroStates :: STRef s [Int],
    _csD             :: STRef s Double,
    _csStateSpace    :: JointStateSpace
}

data ModelEvent = ModelEvent {
    meTime      :: Double,
    meEventType ::EventType
} deriving (Show, Read)

data EventType = Join Int Int
               | Split Int Int Double
               | SetPopSize Int Double
               | SetFreeze Int Bool
               deriving (Show, Read)

data ModelSpec = ModelSpec {
    mTimeSteps      :: [Double],
    mTheta          :: Double,
    mDiscoveryRates :: [Double],
    mPopSizeRegularization :: Double,
    mEvents         :: [ModelEvent]
} deriving (Show)

data ModelState s = ModelState {
    _msT              :: STRef s Double,
    _msEventQueue     :: STRef s [ModelEvent],
    _msPopSize        :: VM.MVector s Double,
    _msFreezeState    :: VM.MVector s Bool
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
    let nVec' = padGhostPops nVec
        config' = padGhostPops config
    let timeSteps = mTimeSteps modelSpec
        d = runST $ do
            ms <- makeInitModelState modelSpec nrPops
            cs <- makeInitCoalState nVec' config'
            propagateStates ms cs timeSteps noShortcut
            readSTRef (_csD cs)
        combFac = product $ zipWith choose nVec' config'
        discoveryRateFactor =
            product [if c > 0 then d else 1.0 |
                (c, d) <- zip config' (mDiscoveryRates modelSpec)]
    assertErr err $ combFac > 0
    --trace (show $ combFac) $ return ()
    return $ d * mTheta modelSpec * combFac * discoveryRateFactor
  where
    err = "Overflow Error in getProb for nVec=" ++ show nVec ++ ", kVec=" ++
        show config
    padGhostPops a =
        let d = nrPops - length a
        in  a ++ replicate d 0
    nrPops = length . mDiscoveryRates $ modelSpec


makeInitModelState :: ModelSpec -> Int -> ST s (ModelState s)
makeInitModelState (ModelSpec _ _ _ _ events) nrPop = do
    sortedEvents <- newSTRef $
        sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) ->
                time1 `compare` time2) events
    t <- newSTRef 0.0
    popSize <- VM.replicate nrPop 1.0
    freezeState <- VM.replicate nrPop False
    return $ ModelState t sortedEvents popSize freezeState

makeInitCoalState :: [Int] -> [Int] -> ST s (CoalState s)
makeInitCoalState nVec config = do
    a <- V.thaw . V.fromList $ zipWith (\n c -> fromIntegral $ n - c) nVec config
    let initialState = V.fromList config
        maxAf = sum config
        nrPop = length nVec
        jointStateSpace = makeJointStateSpace nrPop maxAf
        initialId = _jsStateToId jointStateSpace initialState
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
    if useShortcut && not noShortcut then propagateStateShortcut ms cs else do
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
    e <- readSTRef $ _msEventQueue ms
    return $ allDerivedHaveCoalesced && allAncestralHaveCoalesced && null e

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
            modifySTRef (_msT ms) (+deltaT)

-- debugOutput :: ModelState s -> CoalState s -> Double -> ST s ()
-- debugOutput ms cs nextTime = do
--     currentTime <- readSTRef (_msT ms)
--     aVec <- V.freeze (_csA cs)
--     nonZeroStates <- readSTRef (_csNonZeroStates cs)
--     bList <- mapM (VM.read (_csB cs)) nonZeroStates
--     d <- readSTRef (_csD cs)
--     trace (show nextTime ++ " " ++ show currentTime ++ " " ++ show aVec ++ " " ++
--            show (zip nonZeroStates bList) ++ " " ++ show d) (return ())

performEvent :: ModelState s -> CoalState s -> ST s ()
performEvent ms cs = do
    events <- readSTRef (_msEventQueue ms)
    let ModelEvent _ e = head events
    -- t <- use $ _1 . msT
    -- trace ("time: " ++ show t ++ ": performing event " ++ show (head events)) $ return ()
    case e of
        Join k l -> popJoin ms cs k l
        Split k l m -> popSplit cs k l m
        SetPopSize k p -> VM.write (_msPopSize ms) k p
        SetFreeze k b -> VM.write (_msFreezeState ms) k b
    writeSTRef (_msEventQueue ms) $ tail events

popJoin :: ModelState s -> CoalState s -> Int -> Int -> ST s ()
popJoin ms cs k l = do
    popJoinA (_csA cs) k l
    popJoinB (_csB cs) (_csBtemp cs) (_csNonZeroStates cs) (_csStateSpace cs)
        k l
  where
    deleteMigrations pop list =
        [mig | mig@(k', l', _) <- list, k' /= pop, l' /= pop]

popJoinA :: VM.MVector s Double -> Int -> Int -> ST s ()
popJoinA aVec k l = do
    al <- VM.read aVec l
    ak <- VM.read aVec k
    VM.write aVec l 0.0
    VM.write aVec k (al + ak)

popJoinB :: VM.MVector s Double -> VM.MVector s Double -> STRef s [Int] ->
    JointStateSpace -> Int -> Int -> ST s ()
popJoinB bVec bVecTemp nonZeroStateRef stateSpace k l = do
    VM.set bVecTemp 0.0
    nonZeroStateIds <- readSTRef nonZeroStateRef
    newNonZeroStateIds <- forM nonZeroStateIds $ \xId -> do
        oldProb <- VM.read bVec xId
        let xVec = _jsIdToState stateSpace xId
            xl = xVec V.! l
            xk = xVec V.! k
            xVec' = xVec V.// [(k, xk + xl), (l, 0)]
            xId' = _jsStateToId stateSpace xVec'
        val <- VM.read bVecTemp xId'
        VM.write bVecTemp xId' (val + oldProb)
        return xId'
    VM.copy bVec bVecTemp
    writeSTRef nonZeroStateRef $ getNonZeroStates stateSpace (nub newNonZeroStateIds)

popSplit :: CoalState s -> Int -> Int -> Double -> ST s ()
popSplit cs k l m = do
    popSplitA (_csA cs) k l m
    popSplitB (_csB cs) (_csBtemp cs) (_csNonZeroStates cs) (_csStateSpace cs) k l m

popSplitA :: VM.MVector s Double -> Int -> Int -> Double -> ST s ()
popSplitA aVec k l m = do
    al <- VM.read aVec l
    ak <- VM.read aVec k
    VM.write aVec l $ (1.0 - m) * al
    VM.write aVec k (m * al + ak)

popSplitB :: VM.MVector s Double -> VM.MVector s Double -> STRef s [Int] -> JointStateSpace ->
             Int -> Int -> Double -> ST s ()
popSplitB bVec bVecTemp nonZeroStateRef stateSpace k l m = do
    VM.set bVecTemp 0.0
    nonZeroStateIds <- readSTRef nonZeroStateRef
    newNonZeroStateIds <- fmap concat . forM nonZeroStateIds $ \xId -> do
        oldProb <- VM.read bVec xId
        let xVec = _jsIdToState stateSpace xId
            xl = xVec V.! l
            xk = xVec V.! k
        forM [0..xl] $ \s -> do
            let xVec' = xVec V.// [(k, xk + s), (l, xl - s)]
                xId' = _jsStateToId stateSpace xVec'
            val <- VM.read bVecTemp xId'
            let binomialFactor = m ^ s * (1.0 - m) ^ (xl - s) * choose xl s
            VM.write bVecTemp xId' (val + oldProb * binomialFactor)
            return xId'
    VM.copy bVec bVecTemp
    filteredNonZeroStates <- filterM (VM.read bVec >=> (\x -> return $ x>0.0)) newNonZeroStateIds
    writeSTRef nonZeroStateRef $ getNonZeroStates stateSpace (nub filteredNonZeroStates)

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
        let x1ups = _jsX1up stateSpace xId
            x = _jsIdToState stateSpace xId
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

validateModel :: ModelSpec -> Either String ()
validateModel (ModelSpec _ _ dr reg events) = do
    when (or [t < 0 | ModelEvent t _ <- events]) $ Left "Negative event times"
    when (or [r <= 0 || r > 1 | r <- dr]) $ Left "illegal discovery Rate"
    let sortedEvents =
            sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2) events
    checkEvents sortedEvents
    -- when (reg > 1.0) $ checkRegularization (length dr) reg sortedEvents
  where
    checkEvents [] = Right ()
    checkEvents (ModelEvent _ (Join k l):rest) = do
        let illegalEvents = or $ do
                ModelEvent _ e <- rest
                case e of
                    Join k' l'           -> return $ k' == l || l' == l
                    Split k' l' _        -> return $ k' == l || l' == l
                    SetPopSize k' _      -> return $ k' == l
                    SetFreeze k' _       -> return $ k' == l
        if k == l || illegalEvents then Left "Illegal joins" else checkEvents rest
    checkEvents (ModelEvent _ (SetPopSize _ p):rest) =
        if p <= 0 then Left $ "Illegal population size: " ++ show p else checkEvents rest
    checkEvents (ModelEvent _ (Split _ _ m):rest) =
        if m <= 0.0 || m >= 1.0 then Left "Illegal split rate" else checkEvents rest
    checkEvents (ModelEvent _ (SetFreeze _ _):rest) = checkEvents rest

checkRegularization :: Int -> Double -> [ModelEvent] -> Either String ()
checkRegularization nPops reg sortedEvents =
    let popSizes = V.replicate nPops 1.0
    in  go popSizes sortedEvents
  where
    go _ [] = Right ()
    go ps (ModelEvent t (SetPopSize k newP):rest) =
        let newPs = ps V.// [(k, newP)]
            oldP = ps V.! k
        in  if t /= 0.0 && (((oldP / newP) > reg) ||
                            ((oldP / newP) < (1.0 / reg)))
            then Left ("illegal population size change in branch " ++ show k ++
                       " from " ++ show oldP ++ " to " ++ show newP)
            else go newPs rest
    go ps (ModelEvent t (Join l k):rest) =
        let fromP = ps V.! k
            toP = ps V.! l
        in  if ((fromP / toP) > reg) || ((fromP / toP) < (1.0 / reg))
            then Left ("illegal population size change within join from \
                       \branch " ++ show k ++ " (" ++ show fromP ++
                       ") to branch " ++ show l ++ " (" ++ show toP ++ ")")
            else go ps rest
    go ps (_:rest) = go ps rest

getRegularizationPenalty :: ModelSpec -> Either String Double
getRegularizationPenalty ms = do
    nPops <- getNrOfPops (mEvents ms)
    let initialPopSizes = V.replicate nPops 1.0
    return $ go 0 initialPopSizes sortedEvents
  where
    go res _ [] = res
    go res ps (ModelEvent t (SetPopSize k newP):rest) =
        let newPs = ps V.// [(k, newP)]
            oldP = ps V.! k
            newRes = if t /= 0.0 then res + regFunc reg oldP newP else res
        in  go newRes newPs rest
    go res ps (ModelEvent t (Join l k):rest) =
        let fromP = ps V.! k
            toP = ps V.! l
            newRes = res + regFunc reg fromP toP
        in  go newRes ps rest
    go res ps (_:rest) = go res ps rest
    sortedEvents = sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2)
        (mEvents ms)
    reg = mPopSizeRegularization ms
    regFunc reg oldP newP = if newP > oldP
                            then reg * (newP / oldP - 1.0)^2
                            else reg * (oldP / newP - 1.0)^2

choose :: Int -> Int -> Double
choose _ 0 = 1
choose n k = product [fromIntegral (n + 1 - j) / fromIntegral j | j <- [1..k]]

-- see https://en.wikipedia.org/wiki/Binomial_coefficient
chooseCont :: Double -> Int -> Double
chooseCont _ 0 = 1
chooseCont n k = product [(n + 1.0 - fromIntegral j) / fromIntegral j | j <- [1..k]]

getNrOfPops :: [ModelEvent] -> Either String Int
getNrOfPops modelEvents =
    let maxBranch = maximum allBranches
        nrBranches = length . nub $ allBranches
    in  if maxBranch + 1 /= nrBranches
        then Left "Error: Branch indices are not consecutive. Ghost branch \
            \indices have to be zero-indexed and start with one higher than \
            \the last named branch. Example with four named populations and 1 \
            \ghost populations: the ghost population should have index 4."
        else Right nrBranches
  where
      allBranches = do
          ModelEvent t e <- modelEvents
          case e of
              Join k l -> [k, l]
              Split k l _ -> [k, l]
              SetPopSize k _ -> [k]
              SetFreeze k _ -> [k]
