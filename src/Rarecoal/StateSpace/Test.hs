module Rarecoal.StateSpace.Test (tests, stateSpace, genPopIndex, genRestrictedIds, nrPops, maxAf, genStates) where
    
import Rarecoal.StateSpace (JointStateSpace(..), makeJointStateSpace, JointState, fillUpStateSpace)
import Rarecoal.Utils (computeAllConfigs, computeAllConfigsCrude)

import Control.Monad (replicateM, forM_)
import Data.List (nub)
import qualified Data.Vector.Unboxed as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, Assertion, assertBool)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

tests :: TestTree
tests = testGroup "StateSpace Tests" [
    testProperty "invariant: from id to state and back" prop_idToStateAndBack,
    testProperty "invariant: from state to id and back" prop_stateToIdAndBack,
    testCase "all patterns consistent between different calculations" assertAllConfigConsistency,
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
genStates = (V.fromList <$> replicateM nrPops genAf) `suchThat`
    (\v -> V.sum v <= maxAf && V.sum v > 0)

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

assertAllConfigConsistency :: Assertion
assertAllConfigConsistency = do
    let nVec = [20, 20, 2, 20, 20]
    let allConfigsCrude = computeAllConfigsCrude 5 nVec
        allConfigsNew = computeAllConfigs 5 nVec
    assertBool "different number of patterns with old and new calculation" $
        length allConfigsCrude == length allConfigsCrude
    assertBool "duplicates in old calculation" $
        length allConfigsCrude == length (nub allConfigsCrude)
    assertBool "duplicates in new calculation" $
        length allConfigsNew == length (nub allConfigsNew)
    forM_ allConfigsCrude $ \pat -> 
        assertBool "Old Pattern not found in new patterns" $ pat `elem` allConfigsNew
    forM_ allConfigsNew $ \pat -> 
        assertBool "New Pattern not found in old patterns" $ pat `elem` allConfigsCrude

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
