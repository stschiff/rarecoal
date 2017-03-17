module Rarecoal.ModelTemplate.Test (tests) where

import Rarecoal.Core (defaultTimes)
import Rarecoal.ModelTemplate (ModelTemplate(..), EventTemplate(..), readModelTemplate, ConstraintTemplate(..))

import Control.Error (runScript)
import Test.Tasty
import Test.Tasty.HUnit (testCase, Assertion, assertEqual)

template :: ModelTemplate
template = ModelTemplate {
    mtParams = ["p_EUR", "p_SEA", "p_SIB", "p_FAM", "t_EUR_SIB", "t_SEA_FAM",
        "t_EUR_SEA", "p_EUR_SIB", "p_SEA_FAM", "p_EUR_SEA", "tAdm_FAM_SIB",
        "pAdm_FAM_SIB", "adm_FAM_SIB", "adm_EUR_FAM", "tAdm_SEA_SIB",
        "pAdm_SEA_SIB", "adm_SEA_SIB"],
    mtTheta = 0.0005,
    mtTimeSteps = defaultTimes,
    mtDiscoveryRate = [],
    mtEventTemplates = [
        PopSizeEventTemplate (Left 0.0) (Right "EUR") (Right "p_EUR"),
        PopSizeEventTemplate (Left 0.0) (Right "SEA") (Right "p_SEA"),
        PopSizeEventTemplate (Left 0.0) (Right "SIB") (Right "p_SIB"),
        PopSizeEventTemplate (Left 0.0) (Right "FAM") (Right "p_FAM"),
        JoinPopSizeEventTemplate (Right "t_EUR_SIB") (Right "EUR")
            (Right "SIB") (Right "p_EUR_SIB"),
        JoinPopSizeEventTemplate (Right "t_SEA_FAM") (Right "SEA")
            (Right "FAM") (Right "p_SEA_FAM"),
        JoinPopSizeEventTemplate (Right "t_EUR_SEA") (Right "EUR")
            (Right "SEA") (Right "p_EUR_SEA"),
        SplitEventTemplate (Right "tAdm_FAM_SIB") (Right "FAM") (Right "SIB")
            (Right "adm_FAM_SIB"),
        PopSizeEventTemplate (Right "tAdm_FAM_SIB") (Right "FAM")
            (Right "pAdm_FAM_SIB"),
        SplitEventTemplate (Right "tAdm_SEA_SIB") (Right "SEA") (Right "SIB")
            (Right "adm_SEA_SIB"),
        PopSizeEventTemplate (Right "tAdm_SEA_SIB") (Right "SEA")
            (Right "pAdm_SEA_SIB"),
        SplitEventTemplate (Left 0.00022) (Right "EUR") (Right "FAM")
            (Right "adm_EUR_FAM")],
    mtPopSizeReg = 10,
    mtConstraintTemplates = [
        GreaterConstraintTemplate (Right "tAdm_FAM_SIB") (Left 0.00022)]
}

testLoadTemplate :: Assertion
testLoadTemplate = do
    t <- runScript $ readModelTemplate "exampleData/fourPop.template.txt" 0.0005
        defaultTimes 10
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
