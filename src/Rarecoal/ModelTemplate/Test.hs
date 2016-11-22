module Rarecoal.ModelTemplate.Test (tests) where

import Rarecoal.Core (defaultTimes)
import Rarecoal.ModelTemplate (ModelTemplate(..), EventTemplate(..), readModelTemplate)

import Control.Error (runScript)
import Test.Tasty
import Test.Tasty.HUnit (testCase, Assertion, assertEqual)

template :: ModelTemplate
template = ModelTemplate {
    mtParams = ["p0","p1","p2","p3","p4","t01","t23","t24","t02","p01","p23","p24","p02"],
    mtTheta = 0.0005,
    mtTimeSteps = defaultTimes,
    mtDiscoveryRate = [],
    mtEventTemplates = [ PopSizeEventTemplate (Left 0.0) (Left 0) (Right "p0")
                       , PopSizeEventTemplate (Left 0.0) (Left 1) (Right "p1")
                       , PopSizeEventTemplate (Left 0.0) (Left 2) (Right "p2")
                       , PopSizeEventTemplate (Left 0.0) (Left 3) (Right "p3")
                       , PopSizeEventTemplate (Left 0.0) (Left 4) (Right "p4")
                       , JoinPopSizeEventTemplate (Right "t01") (Left 0) (Left 1) (Right "p01")
                       , JoinPopSizeEventTemplate (Right "t23") (Left 2) (Left 3) (Right "p23")
                       , JoinPopSizeEventTemplate (Right "t24") (Left 2) (Left 4) (Right "p24")
                       , JoinPopSizeEventTemplate (Right "t02") (Left 0) (Left 2) (Right "p02")
                       ],
    mtConstraintTemplates = []
}

testLoadTemplate :: Assertion
testLoadTemplate = do
    t <- runScript $ readModelTemplate "testData/5popSimTemplate.txt" 0.0005 defaultTimes
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
