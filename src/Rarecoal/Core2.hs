{-# LANGUAGE OverloadedStrings #-}
module Rarecoal.Core (getProb, validateModel,
    choose, ModelEvent(..), EventType(..), ModelSpec(..), popJoinA,
    popJoinB, popSplitA, popSplitB, getRegularizationPenalty) where

import           Rarecoal.StateSpace         (JointStateSpace (..),
                                              getNonZeroStates,
                                              makeJointStateSpace, ModelState(..), ModelEvent(..),
                                              validateModel, getNrOfPops, getRegularizationPenalty,
                                              EventType(..), ModelSpec(..), makeInitModelState)
import Rarecoal.Utils (choose, chooseCont)
import           Control.Error.Safe          (assertErr)
import           Control.Exception.Base      (assert)
import           Control.Monad               (filterM, foldM, forM, forM_, when,
                                              (>=>))
import           Control.Monad.ST            (ST, runST)
import           Data.List                   (nub)
import           Data.STRef                  (STRef, modifySTRef, newSTRef,
                                              readSTRef, writeSTRef)
import qualified Data.Text as T
import Turtle (format, (%), w)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
-- import Debug.Trace (trace)

type VecDoub = V.Vector Double
type VecDoubM s = VM.Vector s Double

data ModelState s = ModelState {
    msA              :: VecDoubM s,
    msB              :: VecDoubM s,
    msBtemp          :: VecDoubM s,
    msD              :: STRef s Double,
    msT              :: STRef s Double,
    msNonZeroStates  :: STRef s [Int],
    msStateSpace     :: JointStateSpace,
    msEventQueue     :: STRef s [ModelEvent],
    msPopSize        :: VecDoubM s,
    msFreezeState    :: VM.MVector s Bool
}

getProb :: ModelSpec -> [Int] -> [Int] -> Either T.Text Double
getProb modelSpec nVec config = do
    validateModel modelSpec
    let nrPops = mNrPops modelSpec
    assertErr "illegal sample configuration given" $
        length nVec == length config && length nVec == nrPops
    let dd = runST $ do
            ms <- makeInitState modelSpec nVec config
            propagateStates ms
            readSTRef (_csD cs)
        combFac = product $ zipWith choose nVec config
        discoveryRateFactor =
            product [if c > 0 then d' else 1.0 |
                (c, d') <- zip config (mDiscoveryRates modelSpec)]
    assertErr err $ combFac > 0
    --trace (show $ combFac) $ return ()
    return $ dd * mTheta modelSpec * combFac * discoveryRateFactor
  where
    err = format ("Overflow Error in getProb for nVec="%w%", kVec="%w) nVec config

makeInitState :: ModelSpec -> [Int] -> [Int] -> ST s (ModelState s)
makeInitState modelSpec nVec config = do
    a <- V.thaw . V.fromList $ nVec
    let initialState = V.fromList config
        maxAf = sum config
        nrPop = length nVec
        jointStateSpace = makeJointStateSpace nrPop maxAf
        initialId = _jsStateToId jointStateSpace initialState
    b <- VM.replicate (_jsNrStates jointStateSpace) 0.0
    bTemp <- VM.new (_jsNrStates jointStateSpace)
    -- trace (show ("makeInitCoalState", _jsNrStates jointStateSpace, initialState, initialId)) $
    --        return ()
    VM.write b initialId 1.0
    nonZeroStates <- newSTRef (getNonZeroStates jointStateSpace [initialId])
    bTemp <- VM.new (_jsNrStates jointStateSpace)
    dd <- newSTRef 0.0
    t <- newSTRef 0.0
    sortedEvents <- newSTRef $
        sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) ->
                time1 `compare` time2) (mEvents modelSpec)
    popSize <- VM.replicate nrPop 1.0
    freezeState <- VM.replicate nrPop False
    return $ ModelState a b bTemp dd t nonZeroStates jointStateSpace eventQ popSize freezeState

propagateStates :: ModelState s -> ST s ()
propagateStates ms@(ModelState aVec bVec bVecTemp d t nonZeroStates stateSpace eventQ _ _) = do
    currentT <- readSTRef t
    eq <- readSTRef eventQ
    case eq of
        [] -> propagateToInfinity ms
        (ModelEvent nextT _: rest) -> do
            let deltaT = nextT - currentT
            aOld <- V.freeze $ aVec
            propagateA ms deltaT
            propagateB ms aOld
            writeSTRef (msT ms) nextT
            performEvent ms
            propagateStates ms

propagateToInfinity :: ModelState s -> ST s ()
propagateToInfinity ms = do
    allHaveCoalesced <- checkIfAllHaveCoalesced ms
    when (not $ allHaveCoalesced ms) $ error "model specification results in uncoalesced lineages. \
        \Check whether all branches join at some point"
    let stateSpace = msStateSpace ms
        idToState = _jsIdToState stateSpace
    a <- V.freeze (msA ms)
    let nrA = V.sum a
        popIndex = fst . head . filter ((>0.0) . snd) . zip [0..] . V.toList $ a
    popSize <- VM.read (msPopSize ms) popIndex
    nonZeroIds <- readSTRef (msNonZeroStates ms)
    let nonZeroStates = map idToState nonZeroIds
    probs <- mapM (VM.read (msB cs)) nonZeroIds
    let additionalBranchLength =
            sum [prob * goState popSize nrA state | (state, prob) <- zip nonZeroStates probs]
    -- trace ("shortcut: " ++ show additionalBranchLength ++ "; " ++ show nrA ++ "; " ++
    --        show (zip nonZeroStates probs)) (return ())
    VM.set (msB ms) 0.0
    modifySTRef (msD ms) (+additionalBranchLength)
    VM.write (msA ms) popIndex 1.0
  where
    goState popSize nrA x =
        let nrDerived = assert ((V.length . V.filter (>0)) x == 1) $ V.sum x
        in  singlePopMutBranchLength popSize nrA nrDerived
       
checkIfAllHaveCoalesced :: ModelState s -> ST s Bool
checkIfAllHaveCoalesced ms = do
    nonZeroStates <- readSTRef (msNonZeroStates ms)
    let idToState = _jsIdToState . msStateSpace $ ms
        allDerivedHaveCoalesced =
            and [(V.length . V.filter (>0)) (idToState xId) == 1 | xId <- nonZeroStates]
    a <- V.freeze $ msA cs
    let allAncestralHaveCoalesced = (V.length . V.filter (>0.0)) a == 1
    e <- readSTRef $ msEventQueue ms
    return $ allDerivedHaveCoalesced && allAncestralHaveCoalesced && null e

singlePopMutBranchLength :: Double -> Double -> Int -> Double
singlePopMutBranchLength popSize nrA nrDerived =
    let withCombinatorics = 2.0 * popSize / fromIntegral nrDerived
        combFactor = chooseCont (nrA + fromIntegral nrDerived) nrDerived
    in  withCombinatorics / combFactor

propagateA :: ModelState s -> Double -> ST s ()
propagateA ms deltaT = do
    nrPop <- VM.length (msA ms)
    forM_ [0 .. nrPop - 1] $ \k -> do
        popSizeK <- VM.read (msPopSize ms) k
        freezeStateK <- VM.read (msFreezeState ms) k
        aK <- VM.read (msA ms) k
        let newA = if freezeStateK || aK < 1.0 then aK else propagateSingleA deltaT popSizeK aK
        VM.write (msA ms) k newA

propagateSingleA :: Double -> Double -> Double -> Double
propagateSingleA deltaT popSize a0 =
    1.0 / (1.0 + ((1.0 / a0) - 1.0) * exp (- 0.5 * deltaT / popSize))

propagateB :: ModelState s -> VecDoub -> ST s ()
propagateB ms aVecOld = do
    let nrPop = _jsNrPop $ msStateSpace ms
    nonZeroStateIds <- readSTRef (msNonZeroStates ms)
    VM.set (msBtemp ms) 0.0
    popSizeVec <- V.freeze $ msPopSize ms
    aVec <- V.freeze (msA ms)
    forM_ nonZeroStateIds $ \xId -> do
        let xVecOld = _jsIdToState (msStateSpace ms) xId
        prob <- VM.read (msB ms) xId
        mutBranchInf = computeMutBranchInfForState xVecOld aVecOld popSizeVec
        mutBranchSubtract <- forM nonZeroStateIds $ \xIdNew -> do
            let xVec = _jsIdToState (msStateSpace ms) xIdNew
                rFactor = computeRfactorVec xVec aVec xVecOld aVecOld
            VM.modify bTemp xIdNew (\v -> v + rFactor * prob)
            if mutBranchInf > 0.0
            then return $ rFactor * computeMutBranchInfForState xVec aVecOld popSizeVec
            else return 0.0
        modifySTRef dRef (\v -> v + prob * (mutBranchInf - sum mutBranchSubtract))

computeMutBranchInfForState :: JointState -> VecDoub -> VecDoub -> Double
computeMutBranchInfForState xVec aVec popSize = 
    let sumDerived = V.sum xVec
        maxDerived = V.maximum xVec
        maxIndex = V.maxIndex xVec
    in  if sumDerived == maxDerived
        then singlePopMutBranchLength (popSize V.! maxIndex) (aVec V.! maxIndex) sumDerived
        else 0.0

computeRfactorVec :: JointState -> VecDoub -> JointState -> VecDoub
computeRfactorVec xVec aVec xVecOld aVecOld =
    product . V.toList $ V.zipWith4 computeRfactorCont xVec aVec xVecOld aVecOld

computeRfactorCont :: Int -> Double -> Int -> Double
computeRfactorCont x a xP aP =
    let a0 = floor a
        a1 = a0 + 1
        aP0 = floor aP
        aP1 = aP0 + 1
        vec11 = a1 - a
        vec12 = a - a0
        vec21 = aP1 - aP
        vec22 = aP - aP0
        mat11 = rFac x a0 xP aP0
        mat12 = rFac x a0 xP aP1
        mat21 = rFac x a1 xP aP0
        mat22 = rFac x a1 xP aP1
    in  vec11 * (mat11 * vec21 + mat12 * vec22) + vec12 * (mat21 * vec21 + mat22 * vec22)

rFac :: Int -> Int -> Int -> Int
rFac x a xP aP | x == xP && a == aP = 1
               | xP < x || (aP - xP) < (a - x) = 0
               | otherwise =
                   term1 * rFac x a (xP - 1) aP + term2 * rFac x a xP (aP - 1)
  where
    term1 = fromIntegral (xP * (xP - 1)) / fromIntegral (aP * (aP - 1))
    term2 = fromIntegral ((aP - xP) * (aP - xP - 1)) / fromIntegral (aP * (aP - 1))
        
performEvent :: ModelState s -> ST s ()
performEvent ms = do
    events <- readSTRef (msEventQueue ms)
    let ModelEvent _ e = head events
    -- t <- use $ _1 . msT
    -- trace ("time: " ++ show t ++ ": performing event " ++ show (head events)) $ return ()
    case e of
        Join k l -> do
            popJoinA (msA ms) k l
            popJoinB (msB ms) (msBtemp ms) (msNonZeroStates ms) (msStateSpace ms) k l
        Split k l m -> do
            popSplitA (msA ms) k l m
            popSplitB (msB ms) (msNonZeroStates ms) (msStateSpace ms) k l m
        SetPopSize k p -> VM.write (msPopSize ms) k p
        SetFreeze k b -> VM.write (msFreezeState ms) k b
    writeSTRef (msEventQueue ms) $ tail events

popJoinA :: VecDoubM s -> Int -> Int -> ST s ()
popJoinA aVec k l = do
    al <- VM.read aVec l
    ak <- VM.read aVec k
    VM.write aVec l 0.0
    VM.write aVec k (al + ak)

popJoinB :: VecDoubM s -> VecDoubM s -> STRef s [Int] -> JointStateSpace -> Int -> Int -> ST s ()
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

popSplitA :: VecDoubM s -> Int -> Int -> Double -> ST s ()
popSplitA aVec k l m = do
    al <- VM.read aVec l
    ak <- VM.read aVec k
    VM.write aVec l $ (1.0 - m) * al
    VM.write aVec k (m * al + ak)

popSplitB :: VecDoubM s -> VecDoubM s -> STRef s [Int] -> JointStateSpace -> Int -> Int ->
    Double -> ST s ()
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

