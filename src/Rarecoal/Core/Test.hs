module Rarecoal.Core.Test (tests) where

import qualified Rarecoal.Core as C
import qualified Rarecoal.Core2 as C2
import Rarecoal.Utils (defaultTimes)
import Rarecoal.StateSpace (JointStateSpace(..), ModelEvent(..), EventType(..), ModelSpec(..))
import Rarecoal.StateSpace.Test (stateSpace, genPopIndex, genRestrictedIds, nrPops, maxAf, genStates)

import Control.Monad (replicateM, forM_)
import Control.Monad.ST (runST, ST)
import Data.STRef (newSTRef, STRef)
import Data.List (nub)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Debug.Trace (trace)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase, Assertion, assertBool)
import Test.Tasty (TestTree, testGroup)
import Test.QuickCheck

tests :: TestTree
tests = testGroup "Core Tests" [
    testProperty "joinPopsA leaves total nr of ancestral alleles invariant"
        prop_joinPopsA,
    testProperty "joinPopsB leaves total probabilities invariant"
        prop_joinPopsB,
    testProperty "splitPopsA leaves total nr of ancestral alleles invariant"
        prop_splitPopsA,
    testProperty "splitPopsB leaves total probabilities invariant"
        prop_splitPopsB,
    testProperty "full split equals join for A" prop_fullSplitIsJoinForA,
    testProperty "full split equals join for B" prop_fullSplitIsJoinForB,
    testProperty "Core -> total probabilities are all positive" prop_getProbTest,
    testProperty "Core2 -> total probabilities are all positive" prop_getProb2Test,
    -- testProperty "Core2 -> tupleWrapper tests" prop_rFacTupleTest,
    testCase "testing consistency with previous versions" assertConsistentProbs,
    testCase "testing approximate consistency with previous versions via Core2"
        assertConsistentProbsWithCore2]

prop_joinPopsA :: Property
prop_joinPopsA = forAll (suchThat gen (\(_, k, l) -> k /= l)) go
  where
    gen = (,,) <$> genAVec <*> genPopIndex <*> genPopIndex
    go (aVec, k, l) =
        let aVecNew = runST $ do
                aVecM <- V.thaw aVec
                C.popJoinA aVecM k l
                V.freeze aVecM
        in  abs (V.sum aVec - V.sum aVecNew) < 1.0e-12

genAVec :: Gen (V.Vector Double)
genAVec = V.fromList <$> replicateM nrPops (choose (0.0, 100.0))

genBVec :: Gen (V.Vector Double, [Int])
genBVec = do
    stateIds <- nub <$> replicateM 10 genRestrictedIds
    weights <- replicateM (length stateIds) (choose (0.2, 0.8)) :: Gen [Double]
    let normWeights = [w / sum weights | w <- weights]
        vec = V.replicate (_jsNrStates stateSpace) 0.0 V.// zip stateIds normWeights
    return (vec, stateIds)

prop_joinPopsB :: Property
prop_joinPopsB = forAll (suchThat gen (\(_, _, k, l) -> k /= l)) go
  where
    gen = uncurry (,,,) <$> genBVec <*> genPopIndex <*> genPopIndex
    go :: (V.Vector Double, [Int], Int, Int) -> Bool
    go (bVec, nonZeroStates, k, l) =
        let newBVec = runST $ do
                bVecM <- V.thaw bVec
                bVecTempM <- VM.clone bVecM
                nonZeroStatesRef <- newSTRef nonZeroStates
                C.popJoinB bVecM bVecTempM nonZeroStatesRef stateSpace k l
                V.freeze bVecM
        in (abs (V.sum bVec - V.sum newBVec) < 1.0e-12)

prop_splitPopsA :: Property
prop_splitPopsA = forAll (suchThat gen (\(_, k, l, _) -> k /= l)) go
  where
    gen = (,,,) <$> genAVec <*> genPopIndex <*> genPopIndex <*> choose (0.0, 1.0)
    go (aVec, k, l, m) =
        let aVecNew = runST $ do
                aVecM <- V.thaw aVec
                C.popSplitA aVecM k l m
                V.freeze aVecM
        in  abs (V.sum aVec - V.sum aVecNew) < 1.0e-12

prop_splitPopsB :: Property
prop_splitPopsB = forAll (suchThat gen (\(_, _, k, l, _) -> k /= l)) go
  where
    gen = uncurry (,,,,) <$> genBVec <*> genPopIndex <*> genPopIndex <*> choose (0.0, 1.0)
    go :: (V.Vector Double, [Int], Int, Int, Double) -> Bool
    go (bVec, nonZeroStates, k, l, m) =
        let newBVec = runST $ do
                bVecM <- V.thaw bVec
                bVecTempM <- VM.clone bVecM
                nonZeroStatesRef <- newSTRef nonZeroStates
                C.popSplitB bVecM bVecTempM nonZeroStatesRef stateSpace k l m
                V.freeze bVecM
        in (abs (V.sum bVec - V.sum newBVec) < 1.0e-12)

prop_fullSplitIsJoinForB :: Property
prop_fullSplitIsJoinForB = forAll (suchThat gen (\(_, _, k, l) -> k /= l)) go
  where
    gen = uncurry (,,,) <$> genBVec <*> genPopIndex <*> genPopIndex
    go :: (V.Vector Double, [Int], Int, Int) -> Bool
    go (bVec, nonZeroStates, k, l) = bVec1 == bVec2
      where
        [bVec1, bVec2] = runST $ mapM makeVec twoFuncs
        makeVec :: (VM.MVector s Double -> VM.MVector s Double -> STRef s [Int] ->
                    JointStateSpace -> Int -> Int -> ST s ()) -> ST s (V.Vector Double)
        makeVec func = do
            bVecM <- V.thaw bVec
            bVecTempM <- VM.clone bVecM
            nonZeroStatesRef <- newSTRef nonZeroStates
            func bVecM bVecTempM nonZeroStatesRef stateSpace k l
            V.freeze bVecM
        twoFuncs = [C.popJoinB, \b bT nz s k' l' -> C.popSplitB b bT nz s k' l' 1.0]

prop_fullSplitIsJoinForA :: Property
prop_fullSplitIsJoinForA = forAll (suchThat gen (\(_, k, l) -> k /= l)) go
  where
    gen = (,,) <$> genAVec <*> genPopIndex <*> genPopIndex
    go :: (V.Vector Double, Int, Int) -> Bool
    go (aVec, k, l) = aVec1 == aVec2
      where
        [aVec1, aVec2] = runST $ mapM makeVec twoFuncs
        makeVec :: (VM.MVector s Double -> Int -> Int -> ST s ()) -> ST s (V.Vector Double)
        makeVec func = do
            aVecM <- V.thaw aVec
            func aVecM k l
            V.freeze aVecM
        twoFuncs = [C.popJoinA, \b k' l' -> C.popSplitA b k' l' 1.0]

makeTestModelSpec :: ModelSpec
makeTestModelSpec = ModelSpec 5 defaultTimes 0.0005 [1,1,1,1,1] 10 False events
  where
    events = [ ModelEvent 0.0025 (Join 0 1)
             , ModelEvent 0.006 (Join 2 3)
             , ModelEvent 0.0075 (Join 2 4)
             , ModelEvent 0.01 (Join 0 2)]

prop_getProbTest :: Property
prop_getProbTest = forAll genInput go
  where
    genInput = genStates `suchThat` (\v -> V.sum v > 0 && V.sum v <= maxAf)
    go = get5popProb . V.toList
    get5popProb config = case C.getProb modelSpec nVec config of
        Left _ -> False
        Right res -> res >= 0.0
    modelSpec = makeTestModelSpec
    nVec = [100, 100, 100, 100, 100]

prop_getProb2Test :: Property
prop_getProb2Test = forAll genInput go
  where
    genInput = genStates `suchThat` (\v -> V.sum v > 0 && V.sum v <= maxAf)
    go state = get5popProb (V.toList state)
    get5popProb config = case C2.getProb modelSpec nVec config of
        Left _ -> False
        Right res -> res >= 0.0
    modelSpec = makeTestModelSpec
    nVec = [100, 100, 100, 100, 100]

-- prop_rFacTupleTest :: Property
-- prop_rFacTupleTest = forAll genInput $ \t -> C2.rFacT t == C2.rFacMemoT t
--   where
--     genInput = genRawInput `suchThat` (\(x, a, xP, aP) -> x <= xP && a <= aP)
--     genRawInput = (,,,) <$> genX <*> genA <*> genX <*> genA
--     genX = choose (1, 4)
--     genA = choose (1, 200)

assertConsistentProbs :: Assertion
assertConsistentProbs =
    forM_ resultData $ \(state, previous) -> do
        let current = case C.getProb modelSpec nVec state of
                Left _ -> -1.0
                Right res -> res
        let msg = "failed for state " ++ show state ++ ": " ++ show previous ++
                " (previous) vs. " ++ show current ++ " (current)"
        assertBool msg $ (abs (previous - current) / previous) < 1.0e-8
  where
    resultData = [ ([0,1,2,0,1], 2.009581497800885e-6),
                   ([1,1,1,1,1], 1.5455236177098954e-6),
                   ([2,3,0,0,0], 7.503194796424192e-6),
                   ([1,0,2,3,2], 4.355291335648456e-7),
                   ([0,1,0,0,1], 1.2611453629387214e-5)
                 ]
    modelSpec = makeTestModelSpec
    nVec = [100, 100, 100, 100, 100]

assertConsistentProbsWithCore2 :: Assertion
assertConsistentProbsWithCore2 =
    forM_ resultData $ \(state, previous) -> do
        let current = case C2.getProb modelSpec nVec state of
                Left _ -> -1.0
                Right res -> res
        let msg = "failed for state " ++ show state ++ ": " ++ show previous ++
                " (previous) vs. " ++ show current ++ " (current)"
        -- assuming up to 5 percent deviation between Core1 and Core2 calculations
        assertBool msg $ (abs (previous - current) / previous) < 0.05
  where
    resultData = [ ([0,1,2,0,1], 2.009581497800885e-6),
                   ([1,1,1,1,1], 1.5455236177098954e-6),
                   ([2,3,0,0,0], 7.503194796424192e-6),
                   ([1,0,2,3,2], 4.355291335648456e-7),
                   ([0,1,0,0,1], 1.2611453629387214e-5)
                 ]
    modelSpec = makeTestModelSpec
    nVec = [100, 100, 100, 100, 100]
