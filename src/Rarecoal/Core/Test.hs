module Rarecoal.Core.Test (tests) where

import Rarecoal.Core (ModelEvent(..), EventType(..), ModelSpec(..), joinCounts, popJoinA, popJoinB, getProb, defaultTimes)
import Rarecoal.StateSpace (JointState, JointStateSpace(..))
import Rarecoal.StateSpace.Test (stateSpace, genPopIndex, genRestrictedIds, nrPops, maxAf, genStates)

import Control.Monad (replicateM, forM_)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef)
import Data.List (nub)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase, Assertion, assertBool, assertFailure)
import Test.Tasty (TestTree, testGroup)
import Test.QuickCheck

tests :: TestTree
tests = testGroup "Core Tests" [ testProperty "joinCounts leaves total prob. invariant" 
                                                 prop_joinCountsInvariantAlleles,
                                 testProperty "joinPopsA leaves total nr of ancestral alleles invariant"
                                                 prop_joinPopsA,
                                 testProperty "joinPopsB leaves total probabilities invariant" prop_joinPopsB,
                                 testProperty "total probabilities are all positive" prop_getProbTest,
                                 testCase "testing consistency with previous versions" assert_consistentProbs
                               ]

prop_joinCountsInvariantAlleles :: Property
prop_joinCountsInvariantAlleles = forAll genTriples go
  where
    go :: (Int, Int, Int) -> Bool
    go (k, l, xId) =
        let oldState = (_jsIdToState stateSpace) xId
            newId = joinCounts stateSpace k l xId
            newState = (_jsIdToState stateSpace) newId
        in  V.sum oldState == V.sum newState
    genTriples = suchThat ((,,) <$> genPopIndex <*> genPopIndex <*> genRestrictedIds) (\(k, l, _) -> k /= l)


prop_joinPopsA :: Property
prop_joinPopsA = forAll genTriple go
  where
    genTriple = suchThat ((,,) <$> genVec <*> genPopIndex <*> genPopIndex) (\(_, k, l) -> k /= l)
    genVec = V.fromList <$> replicateM nrPops (choose (0.0, 100.0))
    go (aVec, k, l) =
        let aVecNew = runST $ do
                aVecM <- V.thaw aVec
                popJoinA aVecM k l
                V.freeze aVecM
        in  abs (V.sum aVec - V.sum aVecNew) < 1.0e-8

prop_joinPopsB :: Property
prop_joinPopsB = forAll (suchThat paramGen (\(_, _, k, l) -> k /= l)) go
  where
    paramGen = do
        stateIds <- nub <$> replicateM 10 genRestrictedIds
        weights <- replicateM (length stateIds) (choose (0.2, 0.8)) :: Gen [Double]
        let normWeights = [w / sum weights | w <- weights]
            vec = V.replicate (_jsNrStates stateSpace) 0.0 V.// zip stateIds normWeights
        k <- genPopIndex
        l <- genPopIndex
        return (vec, stateIds, k, l)
    go :: (V.Vector Double, [Int], Int, Int) -> Bool
    go (bVec, nonZeroStates, k, l) =
        let (newBVec, newNonZeroStates) = runST $ do
                bVecM <- V.thaw bVec
                bVecTempM <- VM.clone bVecM
                nonZeroStatesRef <- newSTRef nonZeroStates
                popJoinB bVecM bVecTempM nonZeroStatesRef stateSpace k l
                (,) <$> V.freeze bVecM <*> readSTRef nonZeroStatesRef
        in (abs (V.sum bVec - V.sum newBVec) < 1.0e-8)

makeTestModelSpec :: ModelSpec
makeTestModelSpec = ModelSpec defaultTimes 0.0005 events
  where
    events = [ ModelEvent 0.0025 (Join 0 1)
             , ModelEvent 0.006 (Join 2 3)
             , ModelEvent 0.0075 (Join 2 4)
             , ModelEvent 0.01 (Join 0 2)
             ]

prop_getProbTest :: Property
prop_getProbTest = forAll genInput go
  where
    genInput = (genStates `suchThat` (\v -> V.sum v > 0 && V.sum v <= maxAf))
    go state = get5popProb (V.toList state)
    get5popProb config = case getProb modelSpec nVec False config of
        Left err -> False
        Right res -> res >= 0.0
    modelSpec = makeTestModelSpec
    nVec = [100, 100, 100, 100, 100]

assert_consistentProbs :: Assertion
assert_consistentProbs = do
    forM_ resultData $ \(state, previous) -> do
        let current = case getProb modelSpec nVec False state of
                Left err -> (-1.0)
                Right res -> res
        let msg = "failed for state " ++ show state ++ ": " ++ show previous ++ " (previous) vs. " ++ show current ++ " (current)"
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

-- running prob with this modelSpec
-- rarecoal prob -j 0.0025,0,1 -j 0.006,2,3 -j 0.0075,2,4 -j 0.01,0,2 <NVEC> <CONFIG>