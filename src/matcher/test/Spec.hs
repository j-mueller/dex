{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Cardano.Api           as C
import           Control.Monad.Except  (runExceptT)
import           Data.Either           (isRight)
import qualified Teddy.Matcher.Scripts as Scripts
import           Teddy.Matcher.Scripts (Scripts (..))
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (Assertion, assertBool, assertEqual,
                                        testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "Should be able to load scripts" (runExceptT Scripts.loadScripts >>= assertBool "should be right" . isRight)
  , testCase "Should have the expected script hashes" scriptHashes
  ]

scriptHashes :: Assertion
scriptHashes = do
  Right Scripts{sPoolValidator, sSwapValidator, sDepositValidator, sRedeemValidator} <- runExceptT Scripts.loadScripts
  assertEqual "Pool validator"    "4888c78356d18d2858be5729544626966676b57312d4355b0f4f9535" (C.hashScript sPoolValidator)
  assertEqual "Deposit validator" "93ee3a2a7059146ff9231068b55c84ad6884274e8f8106e36369f5f2" (C.hashScript sDepositValidator)
  assertEqual "Swap validator"    "dc7c2eec5a87a4d972d0405187eec782ae80bea72492cd7b244c3870" (C.hashScript sSwapValidator)
  assertEqual "Redeem validator"  "aeac18c3ffc9e94109c173caa0500015aea23c45773b478754699cee" (C.hashScript sRedeemValidator)
