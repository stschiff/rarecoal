{-# LANGUAGE OverloadedStrings #-}
module RarecoalLib.ModelTemplate.Test (tests) where

import RarecoalLib.ModelTemplate (ModelTemplate(..), MTEvent(..), MTConstraint(..), ParamSpec(..),
    ConstraintOperator(..), getModelTemplate, ModelOptions(..))

import Test.Tasty
import Test.Tasty.HUnit (testCase, Assertion, assertEqual)

template :: ModelTemplate
template = ModelTemplate {
    mtBranchNames = ["EUR", "SEA", "SIB", "FAM"],
    mtEvents = [
        MTPopSizeChange (ParamFixed 0.0) "EUR" (ParamVariable "p_EUR"),
        MTPopSizeChange (ParamFixed 0.0) "SEA" (ParamVariable "p_SEA"),
        MTPopSizeChange (ParamFixed 0.0) "SIB" (ParamVariable "p_SIB"),
        MTPopSizeChange (ParamFixed 0.0) "FAM" (ParamVariable "p_FAM"),
        MTJoinPopSizeChange (ParamVariable "t_EUR_SIB") "EUR" "SIB" (ParamVariable "p_EUR_SIB"),
        MTJoinPopSizeChange (ParamVariable "t_SEA_FAM") "SEA" "FAM" (ParamVariable "p_SEA_FAM"),
        MTJoinPopSizeChange (ParamVariable "t_EUR_SEA") "EUR" "SEA" (ParamVariable "p_EUR_SEA"),
        MTSplit (ParamVariable "tAdm_FAM_SIB") "FAM" "SIB" (ParamVariable "adm_FAM_SIB"),
        MTPopSizeChange (ParamVariable "tAdm_FAM_SIB") "FAM" (ParamVariable "pAdm_FAM_SIB"),
        MTSplit (ParamVariable "tAdm_SEA_SIB") "SEA" "SIB" (ParamVariable "adm_SEA_SIB"),
        MTPopSizeChange (ParamVariable "tAdm_SEA_SIB") "SEA" (ParamVariable "pAdm_SEA_SIB"),
        MTSplit (ParamFixed 0.00022) "EUR" "FAM" (ParamVariable "adm_EUR_FAM")
    ],
    mtConstraints = [
        MTConstraint (ParamVariable "tAdm_FAM_SIB") (ParamFixed 0.00022) ConstraintOpGreater
    ]
}

testLoadTemplate :: Assertion
testLoadTemplate = do
    t <- getModelTemplate (ModelByFile "exampleData/fourPop.template.txt")
    assertEqual "testLoadTemplate" t template

-- testGetNewParams :: Assertion
-- testGetNewParams = do
--     let p = V.fromList [1,1,1,1,1,0.02,0.04,0.041,0.05,1,2,3,4]
--         newP = getNewParams template p 6 0.002
--     assertEqual "switch time with overlapping join" (V.fromList [1, 1, 1, 1, 1, 0.02, 0.042, 0.041, 0.05, 1, 3, 3, 4]) newP
--     let newP' = getNewParams template p 7 0.002
--         diffVec = V.zipWith (\a b -> abs (a - b)) (V.fromList [1, 1, 1, 1, 1, 0.02, 0.04, 0.043, 0.05, 1, 2, 3, 4]) newP'
--     assertBool "no overlapping join" $ V.all (<1.0e-8) diffVec

tests :: TestTree
tests = testGroup "ModelTemplate Tests" [ testCase "testing template loading" testLoadTemplate]
                                        -- , testCase "testing getNewParams" testGetNewParams]
