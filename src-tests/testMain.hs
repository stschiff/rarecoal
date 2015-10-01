{-# LANGUAGE FlexibleInstances #-}

import Rarecoal.StateSpace (makeJointStateSpace, JointState, JointStateSpace(..))
import Rarecoal.Core (getProb, ModelEvent(..), EventType(..), ModelSpec(..), defaultTimes, joinCounts,
                      popJoinA, popJoinB)

import Control.Monad (replicateM, when)
import Control.Monad.ST (runST)
import Data.List (nub)
import Data.STRef (newSTRef, readSTRef)
import qualified Data.Vector.Unboxed as V
import Debug.Trace (trace)
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck prop_idToStateAndBack
    quickCheck prop_stateToIdAndBack
    quickCheck prop_joinCountsInvariantAlleles
    quickCheck prop_joinPopsA
    quickCheck prop_joinPopsB
    quickCheck $ prop_getProbTest

genIds :: Gen Int
genIds = choose (0, nrStates - 1)
  where
    nrStates = _jsNrStates stateSpace

stateSpace :: JointStateSpace
stateSpace = makeJointStateSpace nrPops maxAf

nrPops :: Int
nrPops = 5

maxAf :: Int 
maxAf = 4

prop_idToStateAndBack :: Property
prop_idToStateAndBack = forAll genIds go
  where
    go :: Int -> Bool
    go xId =
        let x = _jsIdToState stateSpace $ xId
            xId' = _jsStateToId stateSpace $ x
        in  xId == xId'
    
genStates :: Gen JointState
genStates = V.fromList <$> replicateM nrPops genAf

genAf :: Gen Int
genAf = choose (0, maxAf)

prop_stateToIdAndBack :: Property
prop_stateToIdAndBack = forAll genStates go
  where
    go :: JointState -> Bool
    go x = 
        let xId = _jsStateToId stateSpace $ x
            x' = _jsIdToState stateSpace $ xId
        in  x == x'

genPopIndex :: Gen Int
genPopIndex = choose (0, nrPops - 1)

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

genRestrictedIds :: Gen Int
genRestrictedIds = genIds `suchThat` (\xId -> V.sum ((_jsIdToState stateSpace) xId) <= maxAf)

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
                bVecTempM <- V.thaw bVec
                nonZeroStatesRef <- newSTRef nonZeroStates
                popJoinB bVecM bVecTempM nonZeroStatesRef stateSpace k l
                (,) <$> V.freeze bVecM <*> readSTRef nonZeroStatesRef
        in (abs (V.sum bVec - V.sum newBVec) < 1.0e-8) && length newNonZeroStates <= length nonZeroStates

prop_getProbTest :: Property
prop_getProbTest = forAll genInput go
  where
    genInput = (genStates `suchThat` (\v -> V.sum v > 0 && V.sum v <= maxAf))
    go :: JointState -> Bool
    go state = get5popProb (V.toList state)
    get5popProb config = case prob of
        Left err -> False
        Right res -> res >= 0.0
      where
        prob = getProb modelSpec nVec False config
        modelSpec = ModelSpec defaultTimes 0.001 events
        events = [ ModelEvent 0.0025 (Join 0 1)
                 , ModelEvent 0.006 (Join 2 3)
                 , ModelEvent 0.0075 (Join 2 4)
                 , ModelEvent 0.01 (Join 0 2)
                 ]
        nVec = [100, 100, 100, 100, 100]

