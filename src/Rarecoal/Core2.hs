{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Rarecoal.Core2 (getProb, getProbWithMemo, validateModel,
    choose, ModelEvent(..), EventType(..), ModelSpec(..), popJoinA,
    -- intToTuple, tupleToInt, getLeftMostDigitWithBase, rFacT, rFacMemoT,
    popJoinB, popSplitA, popSplitB, getRegularizationPenalty, rFac) where

import           Rarecoal.StateSpace         (JointStateSpace (..), JointState, 
                                              fillUpStateSpace,
                                              ModelEvent(..),
                                              validateModel, getRegularizationPenalty,
                                              EventType(..), ModelSpec(..))
import Rarecoal.Utils (choose, chooseCont)
import           Control.Error.Safe          (assertErr)
import           Control.Exception.Base      (assert)
import           Control.Monad               (filterM, forM, forM_, when, (>=>))
import           Control.Monad.ST            (ST, runST)
import           Data.List                   (nub, sortBy)
import           Data.STRef                  (STRef, modifySTRef, newSTRef,
                                              readSTRef, writeSTRef)
import qualified Data.Text as T
-- import Debug.Trace (trace)
import Turtle (format, (%), w)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

type VecDoub = V.Vector Double
type VecDoubM s = VM.MVector s Double

type RFacFunc = Int -> Int -> Int -> Int -> Double

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
    msFreezeState    :: VM.MVector s Bool,
    msRfactorMemo    :: RFacFunc
}

getProbWithMemo :: RFacFunc -> ModelSpec -> JointStateSpace -> [Int] -> [Int] ->
    Either T.Text Double
getProbWithMemo rFacMemo modelSpec jointStateSpace nVec config = do
    validateModel modelSpec
    let nrPops = mNrPops modelSpec
    assertErr "illegal sample configuration given" $
        length nVec == length config && length nVec == nrPops
    let dd = runST $ do
            ms <- makeInitState rFacMemo modelSpec jointStateSpace nVec config
            propagateStates ms
            readSTRef (msD ms)
        combFac = product $ zipWith choose nVec config
        discoveryRateFactor =
            product [if c > 0 then d' else 1.0 |
                (c, d') <- zip config (mDiscoveryRates modelSpec)]
    assertErr err $ combFac > 0
    --trace (show $ combFac) $ return ()
    return $ dd * mTheta modelSpec * combFac * discoveryRateFactor
  where
    err = format ("Overflow Error in getProb for nVec="%w%", kVec="%w) nVec config

getProb :: ModelSpec -> JointStateSpace -> [Int] -> [Int] -> Either T.Text Double
getProb = getProbWithMemo rFac

makeInitState :: RFacFunc -> ModelSpec -> JointStateSpace -> [Int] -> [Int] -> ST s (ModelState s)
makeInitState rFacMemo modelSpec jointStateSpace nVec config = do
    a <- V.thaw . V.map fromIntegral . V.fromList $ nVec
    let initialState = V.fromList config
        nrPop = length nVec
        initialId = _jsStateToId jointStateSpace initialState
    b <- VM.replicate (_jsNrStates jointStateSpace) 0.0
    bTemp <- VM.new (_jsNrStates jointStateSpace)
    -- trace (show ("makeInitCoalState", _jsNrStates jointStateSpace, initialState, initialId)) $
    --        return ()
    VM.write b initialId 1.0
    nonZeroStates <- newSTRef [initialId]
    dd <- newSTRef 0.0
    t <- newSTRef 0.0
    sortedEvents <- newSTRef $
        sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) ->
                time1 `compare` time2) (mEvents modelSpec)
    popSize <- VM.replicate nrPop 1.0
    freezeState <- VM.replicate nrPop False
    return $ ModelState a b bTemp dd t nonZeroStates jointStateSpace sortedEvents popSize 
        freezeState rFacMemo

propagateStates :: ModelState s -> ST s ()
propagateStates ms = do
    currentT <- readSTRef (msT ms)
    eq <- readSTRef (msEventQueue ms)
    case eq of
        [] -> propagateToInfinity ms
        (ModelEvent nextT _: _) -> do
            let deltaT = nextT - currentT
            when (deltaT > 0) $ do
                -- reportState "Before Prop" ms
                aOld <- V.freeze (msA ms)
                accumulateSFS ms deltaT
                propagateA ms deltaT
                propagateB ms aOld
                writeSTRef (msT ms) nextT
                -- reportState "After Prop" ms
            performEvent ms
            -- reportState "After event" ms
            propagateStates ms

-- reportState :: String -> ModelState s -> ST s ()
-- reportState name ms = do
--     aVec <- V.freeze (msA ms)
--     t <- readSTRef (msT ms)
--     d <- readSTRef (msD ms)
--     nonZeroStates <- readSTRef (msNonZeroStates ms)
--     probs <- mapM (\xId -> VM.read (msB ms) xId) nonZeroStates
--     let xs = [_jsIdToState (msStateSpace ms) xId | xId <- nonZeroStates]
--     trace (name ++ ": t=" ++ show t ++ "; d=" ++ show d ++
--         "; aVec=" ++ show aVec ++ "; b=" ++ show (zip xs probs)) (return ())

propagateToInfinity :: ModelState s -> ST s ()
propagateToInfinity ms = do
    allHaveCoalesced <- checkIfAllHaveCoalesced ms
    when (not allHaveCoalesced) $ error "model specification results in uncoalesced lineages. \
        \Check whether all branches join at some point"
    let stateSpace = msStateSpace ms
        idToState = _jsIdToState stateSpace
    a <- V.freeze (msA ms)
    let nrA = V.sum a
        popIndex = fst . head . filter ((>0.0) . snd) . zip [0..] . V.toList $ a
    popSize <- VM.read (msPopSize ms) popIndex
    nonZeroIds <- readSTRef (msNonZeroStates ms)
    let nonZeroStates = map idToState nonZeroIds
    probs <- mapM (VM.read (msB ms)) nonZeroIds
    let additionalBranchLength =
            sum [prob * goState popSize nrA state | (state, prob) <- zip nonZeroStates probs]
    -- trace ("prob " ++ show probs) $ return ()
    -- trace ("additional Branch Length " ++ show additionalBranchLength) $ return ()
    VM.set (msB ms) 0.0
    modifySTRef (msD ms) (+additionalBranchLength)
    VM.write (msA ms) popIndex 1.0
  where
    goState popSize nrA x =
        let nrDerived = assert ((V.length . V.filter (>0)) x == 1) $ V.sum x
        in  sfsCont popSize nrA nrDerived
       
checkIfAllHaveCoalesced :: ModelState s -> ST s Bool
checkIfAllHaveCoalesced ms = do
    nonZeroStates <- readSTRef (msNonZeroStates ms)
    let idToState = _jsIdToState . msStateSpace $ ms
        allDerivedHaveCoalesced =
            and [(V.length . V.filter (>0)) (idToState xId) == 1 | xId <- nonZeroStates]
    a <- V.freeze $ msA ms
    let allAncestralHaveCoalesced = (V.length . V.filter (>0.0)) a == 1
    e <- readSTRef $ msEventQueue ms
    return $ allDerivedHaveCoalesced && allAncestralHaveCoalesced && null e

sfsCont :: Double -> Double -> Int -> Double
sfsCont popSize nrA nrDerived =
    let withCombinatorics = 2.0 * popSize / fromIntegral nrDerived
        combFactor = chooseCont nrA nrDerived
        -- combFactor = choose (max nrDerived (round nrA)) nrDerived
    in  withCombinatorics / combFactor


accumulateSFS :: ModelState s -> Double -> ST s ()
accumulateSFS ms deltaT = do
    nonZeroStates <- readSTRef (msNonZeroStates ms)
    popSizeVec <- V.freeze $ msPopSize ms
    aVec <- V.freeze (msA ms)
    forM_ nonZeroStates $ \xId -> do
        let xVec = _jsIdToState (msStateSpace ms) xId
        prob <- VM.read (msB ms) xId
        let sumDerived = V.sum xVec
            maxDerived = V.maximum xVec
            maxIndex = V.maxIndex xVec
            popSize = popSizeVec V.! maxIndex
            a = aVec V.! maxIndex
            addSFS = if   sumDerived == maxDerived
                     then conditionalSFScont popSize deltaT a sumDerived
                     else 0.0
        modifySTRef (msD ms) (\v -> v + prob * addSFS)

conditionalSFScont :: Double -> Double -> Double -> Int -> Double
conditionalSFScont popSize tau a i = 
    let a0 = floor a
        a1 = a0 + 1
        r0 = conditionalSFS popSize tau a0 i
        r1 = conditionalSFS popSize tau a1 i
    in  (a - fromIntegral a0) * r1 + (fromIntegral a1 - a) * r0

-- see Mathematica notebook for theory
conditionalSFS :: Double -> Double -> Int -> Int -> Double
conditionalSFS popSize tau m i =
    let rawSFS = (2.0 * popSize / fromIntegral i) / choose (m - 1) i * sum_
        combFactor = choose m i
    in  rawSFS / combFactor
  where
    sum_ = sum $ do
        k <- [2..(m - i + 1)]
        return $ choose k 2 * choose (m - k) (i - 1) * interCoalTime m k tau (1.0 / popSize)

interCoalTime :: Int -> Int -> Double -> Double -> Double
interCoalTime m k tau lambda
    | m == k    = 1.0 / (lambda * k2) * (1.0 - exp (-k2 * lambda * tau))
    | otherwise = (interCoalTime (m - 1) k tau lambda - interCoalTime m (k + 1) tau lambda *
        fromIntegral (k * (k + 1)) / fromIntegral (m * (m - 1))) / norm
  where
    k2 = choose k 2
    norm = 1.0 - fromIntegral (k * (k - 1)) / fromIntegral (m * (m - 1))
-----------------------------



propagateA :: ModelState s -> Double -> ST s ()
propagateA ms deltaT = do
    let nrPop = VM.length (msA ms)
    forM_ [0 .. nrPop - 1] $ \k -> do
        popSizeK <- VM.read (msPopSize ms) k
        freezeStateK <- VM.read (msFreezeState ms) k
        aK <- VM.read (msA ms) k
        let newA = if freezeStateK || aK < 1.0 then aK else propagateSingleA deltaT popSizeK aK
        -- trace ("newA=" ++ show newA ++ ", deltaT=" ++ show deltaT) $ return ()
        VM.write (msA ms) k newA

propagateSingleA :: Double -> Double -> Double -> Double
propagateSingleA deltaT popSize a0 =
    1.0 / (1.0 + ((1.0 / a0) - 1.0) * exp (- 0.5 * deltaT / popSize))

propagateB :: ModelState s -> VecDoub -> ST s ()
propagateB ms aVecOld = do
    nonZeroStateIds <- readSTRef (msNonZeroStates ms)
    VM.set (msBtemp ms) 0.0
    aVec <- V.freeze (msA ms)
    -- trace ("aVecOld=" ++ show aVecOld ++ ", aVec=" ++ show aVec) $ return ()
    let rFacMemo = msRfactorMemo ms
    newNonZeroStates <- fmap concat . forM nonZeroStateIds $ \xId -> do
        let xVecOld = _jsIdToState (msStateSpace ms) xId
        prob <- VM.read (msB ms) xId
        if (prob > 0.0) then do
            let allTargetStates = fillUpStateSpace (msStateSpace ms) [xId]
            forM_ allTargetStates $ \xIdNew -> do
                let xVec = _jsIdToState (msStateSpace ms) xIdNew
                    rFactor = computeRfactorVec rFacMemo xVec aVec xVecOld aVecOld
                VM.modify (msBtemp ms) (\v -> v + rFactor * prob) xIdNew 
            return allTargetStates
        else return [] -- this should never be the case, actually.
    writeSTRef (msNonZeroStates ms) (nub newNonZeroStates)
    VM.copy (msB ms) (msBtemp ms) 

computeRfactorVec :: RFacFunc -> JointState -> VecDoub -> JointState -> VecDoub -> Double
computeRfactorVec rFacMemo xVec aVec xVecOld aVecOld =
        product . V.toList $ V.zipWith4 (computeRfactorCont1 rFacMemo) xVec aVec xVecOld aVecOld

computeRfactorCont1 :: RFacFunc -> Int -> Double -> Int -> Double -> Double
computeRfactorCont1 rFacMemo x aCont xP aPcont =
    let aP0 = floor aPcont
        aP1 = aP0 + 1
        prob_aP1 = aPcont - fromIntegral aP0
        prob_aP0 = 1.0 - prob_aP1
    in  prob_aP0 * computeRfactorCont2 rFacMemo x aCont xP aP0 +
        prob_aP1 * computeRfactorCont2 rFacMemo x aCont xP aP1

computeRfactorCont2 :: RFacFunc -> Int -> Double -> Int -> Int -> Double
computeRfactorCont2 rFacMemo x aCont xP aP =
    let a0 = floor aCont
        a1 = a0 + 1
    in  if a0 == aP -- this means that a0 == aP and a1 == aP0 + 1.
        then rFac x a0 xP aP
        else
            case aP of
                1 -> rFac x 1 xP aP
                0 -> rFac x 0 xP aP
                _ -> let prob_a1 = aCont - fromIntegral a0
                         prob_a0 = 1.0 - prob_a1
                     in  prob_a0 * rFacMemo x a0 xP aP + prob_a1 * rFacMemo x a1 xP aP

rFac :: Int -> Int -> Int -> Int -> Double
rFac !x !a !xP !aP = rFacR x a xP aP

rFacR :: Int -> Int -> Int -> Int -> Double
rFacR !x !a !xP !aP | x == xP && a == aP = 1
                   | aP < a = error "rFac called with illegal configuration"
                   | xP < x || (aP - xP) < (a - x) = 0
                   | otherwise = term1 * rFacR x a (xP - 1) (aP - 1) + term2 * rFacR x a xP (aP - 1)
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
            popSplitB (msB ms) (msBtemp ms) (msNonZeroStates ms) (msStateSpace ms) k l m
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
    writeSTRef nonZeroStateRef (nub newNonZeroStateIds)

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
    writeSTRef nonZeroStateRef (nub filteredNonZeroStates)
