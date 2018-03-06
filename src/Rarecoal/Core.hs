{-# LANGUAGE OverloadedStrings #-}
module Rarecoal.Core (getProb, ModelEvent(..), EventType(..), ModelSpec(..), popJoinA,
    popJoinB, popSplitA, popSplitB) where

import           Rarecoal.StateSpace         (JointStateSpace (..),
                                              getNonZeroStates,
                                              makeJointStateSpace, ModelEvent(..),
                                              validateModel,
                                              EventType(..), ModelSpec(..))
import Rarecoal.Utils (choose, chooseCont)
import           Control.Error.Safe          (assertErr)
import           Control.Exception.Base      (assert)
import           Control.Monad               (filterM, foldM, forM, forM_, when,
                                              (>=>))
import           Control.Monad.ST            (ST, runST)
import           Data.List                   (nub)
import           Data.STRef                  (STRef, modifySTRef, newSTRef,
                                              readSTRef, writeSTRef)
import Data.List (sortBy)
import qualified Data.Text as T
import Turtle (format, (%), w)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
-- import Debug.Trace (trace)

data ModelState s = ModelState {
    _msT              :: STRef s Double,
    _msEventQueue     :: STRef s [ModelEvent],
    _msPopSize        :: VM.MVector s Double,
    _msFreezeState    :: VM.MVector s Bool
}

data CoalState s = CoalState {
    _csA             :: VM.MVector s Double,
    _csB             :: VM.MVector s Double,
    _csBtemp         :: VM.MVector s Double,
    _csNonZeroStates :: STRef s [Int],
    _csD             :: STRef s Double,
    _csStateSpace    :: JointStateSpace
}

getProb :: ModelSpec -> [Int] -> [Int] -> Either T.Text Double
getProb modelSpec nVec config = do
    validateModel modelSpec
    let nrPops = mNrPops modelSpec
    assertErr "illegal sample configuration given" $
        length nVec == length config && length nVec == nrPops
    let timeSteps = mTimeSteps modelSpec
        dd = runST $ do
            ms <- makeInitModelState modelSpec
            cs <- makeInitCoalState nVec config
            propagateStates ms cs timeSteps (mNoShortcut modelSpec)
            readSTRef (_csD cs)
        combFac = product $ zipWith choose nVec config
        discoveryRateFactor =
            product [if c > 0 then d' else 1.0 |
                (c, d') <- zip config (mDiscoveryRates modelSpec)]
    assertErr err $ combFac > 0
    --trace (show $ combFac) $ return ()
    return $ dd * mTheta modelSpec * combFac * discoveryRateFactor
  where
    err = format ("Overflow Error in getProb for nVec="%w%", kVec="%w) nVec
        config

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
    dd <- newSTRef 0.0
    return $ CoalState a b bTemp nonZeroStates dd jointStateSpace

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
        Join k l -> popJoin cs k l
        Split k l m -> popSplit cs k l m
        SetPopSize k p -> VM.write (_msPopSize ms) k p
        SetFreeze k b -> VM.write (_msFreezeState ms) k b
    writeSTRef (_msEventQueue ms) $ tail events

popJoin :: CoalState s -> Int -> Int -> ST s ()
popJoin cs k l = do
    popJoinA (_csA cs) k l
    popJoinB (_csB cs) (_csBtemp cs) (_csNonZeroStates cs) (_csStateSpace cs) k l

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

makeInitModelState :: ModelSpec -> ST s (ModelState s)
makeInitModelState (ModelSpec nrPop _ _ _ _ _ events) = do
    sortedEvents <- newSTRef $
        sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) ->
                time1 `compare` time2) events
    t <- newSTRef 0.0
    popSize <- VM.replicate nrPop 1.0
    freezeState <- VM.replicate nrPop False
    return $ ModelState t sortedEvents popSize freezeState

