module Rarecoal.StateSpace.Test (tests, stateSpace, genPopIndex, genRestrictedIds, nrPops, maxAf, genStates) where
    
import Rarecoal.StateSpace (JointStateSpace(..), makeJointStateSpace, JointState, fillUpStateSpace)

import Control.Monad (replicateM, forM_)
import qualified Data.Vector.Unboxed as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, Assertion, assertBool)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

tests :: TestTree
tests = testGroup "StateSpace Tests" [
    testProperty "invariant: from id to state and back" prop_idToStateAndBack,
    testProperty "invariant: from state to id and back" prop_stateToIdAndBack,
    testCase "fillStateSpace correct" assertCorrectFullStateSpace]

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

assertCorrectFullStateSpace :: Assertion
assertCorrectFullStateSpace = do
    let allStates = fillUpStateSpace stateSpace . map (_jsStateToId stateSpace) $
            [V.fromList [2,2,0,0,0], V.fromList [0,2,2,0,0]]
    forM_ allStates $ \x -> do
        assertBool ("failed for fillup-state " ++ show (_jsIdToState stateSpace x) ++
            " which should is not in the predef-list") $ x `elem` xResults
    forM_ xResults $ \x -> do
        assertBool ("failed for predef state " ++ show (_jsIdToState stateSpace x) ++
            " which should be in the fill-up list") $ x `elem` allStates
  where
    xResults = map (_jsStateToId stateSpace . V.fromList) $ [
        [2,2,0,0,0], [1,2,0,0,0], [2,1,0,0,0], [1,1,0,0,0],
        [0,2,2,0,0], [0,1,2,0,0], [0,2,1,0,0], [0,1,1,0,0]]
