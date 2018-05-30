import qualified Rarecoal.Core.Test
import qualified Rarecoal.ModelTemplate.Test
import qualified Rarecoal.StateSpace.Test

import Test.Tasty (defaultMain, TestTree, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Rarecoal.StateSpace.Test.tests,
                           Rarecoal.Core.Test.tests,
                           Rarecoal.ModelTemplate.Test.tests]

