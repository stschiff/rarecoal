{-# LANGUAGE FlexibleInstances #-}

import Rarecoal.StateSpace (makeJointStateSpace, JointState, JointStateSpace(..))
import Rarecoal.Core (getProb, ModelEvent(..), EventType(..), ModelSpec(..), defaultTimes, joinCounts)

import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as V
import Debug.Trace (trace)
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck $ forAll genIds prop_idToStateAndBack
    quickCheck $ forAll genStates prop_stateToIdAndBack
    quickCheck $ forAll genTriples prop_joinCountsInvariantAlleles
    quickCheck $ forAll (genStates `suchThat` (\v -> V.sum v > 0 && V.sum v <= maxAf)) prop_getProbTest

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

prop_idToStateAndBack :: Int -> Bool
prop_idToStateAndBack xId =
    let x = _jsIdToState stateSpace $ xId
        xId' = _jsStateToId stateSpace $ x
    in  xId == xId'

genStates :: Gen JointState
genStates = V.fromList <$> replicateM nrPops genAf

genAf :: Gen Int
genAf = choose (0, maxAf)

prop_stateToIdAndBack :: JointState -> Bool
prop_stateToIdAndBack x =
    let xId = _jsStateToId stateSpace $ x
        x' = _jsIdToState stateSpace $ xId
    in  x == x'

genTriples :: Gen (Int, Int, Int)
genTriples = suchThat ((,,) <$> genPopIndex <*> genPopIndex <*> genRestrictedIds) (\(k, l, _) -> k /= l)
  where
    genRestrictedIds = genIds `suchThat` (\xId -> V.sum ((_jsIdToState stateSpace) xId) <= maxAf)

genPopIndex :: Gen Int
genPopIndex = choose (0, nrPops - 1)

prop_joinCountsInvariantAlleles :: (Int, Int, Int) -> Bool
prop_joinCountsInvariantAlleles (k, l, xId) =
    let oldState = (_jsIdToState stateSpace) xId
        newId = joinCounts stateSpace k l xId
        newState = (_jsIdToState stateSpace) newId
    in  V.sum oldState == V.sum newState

prop_getProbTest :: JointState -> Bool
prop_getProbTest state = get5popProb (V.toList state)
  where
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




