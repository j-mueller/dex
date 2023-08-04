{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Monad.Except  (runExceptT)
import           Data.Either           (isRight)
import qualified Teddy.Matcher.Scripts as Scripts
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (assertBool, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "Should be able to load scripts" (runExceptT Scripts.loadScripts >>= assertBool "should be right" . isRight)
  ]
