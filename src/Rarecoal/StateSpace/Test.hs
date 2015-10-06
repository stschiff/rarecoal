module Rarecoal.StateSpace.Test (tests, stateSpace, genPopIndex, genRestrictedIds, nrPops, maxAf, genStates) where
    
import Rarecoal.StateSpace (JointStateSpace(..), makeJointStateSpace, JointState)

import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

tests :: TestTree
tests = testGroup "StateSpace Tests" [ testProperty "invariant: from id to state and back" prop_idToStateAndBack,
                                       testProperty "invariant: from state to id and back" prop_stateToIdAndBack
                                     ]

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

genRestrictedIds :: Gen Int
genRestrictedIds = genIds `suchThat` (\xId -> V.sum ((_jsIdToState stateSpace) xId) <= maxAf)

