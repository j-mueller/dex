{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Data.Either           (isRight)
import qualified Teddy.Matcher.Scripts as Scripts
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (Assertion, assertBool, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "Should be able to load scripts" (Scripts.loadScripts >>= assertBool "should be right" . isRight)
  ]
