module Spec.Unit(
  tests
) where

import           Convex.MockChain.Utils (mockchainSucceeds)
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (testCase)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "create LQ pool" (mockchainSucceeds createLQPool)
  ]

createLQPool :: Monad m => m ()
createLQPool = pure ()
