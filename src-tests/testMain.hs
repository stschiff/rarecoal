import qualified RarecoalLib.Core.Test
import qualified RarecoalLib.ModelTemplate.Test
import qualified RarecoalLib.StateSpace.Test

import Test.Tasty (defaultMain, TestTree, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [RarecoalLib.StateSpace.Test.tests,
                           RarecoalLib.Core.Test.tests,
                           RarecoalLib.ModelTemplate.Test.tests]

