{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Cardano.Api                 as C
import           Control.Monad.Except        (runExceptT)
import           Data.Either                 (isRight)
import           PlutusLedgerApi.V1.Contexts (TxOutRef (..))
import           PlutusLedgerApi.V1.Value    (TokenName (..))
import qualified Teddy.Matcher.Scripts       as Scripts
import           Teddy.Matcher.Scripts       (Scripts (..))
import           Test.Tasty                  (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit            (Assertion, assertBool,
                                              assertEqual, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests"
  [ testGroup "script hashes"
    [ testGroup "serialised"
      [ testCase "load scripts" (runExceptT Scripts.loadScripts >>= assertBool "should be right" . isRight)
      , testCase "expected script hashes" scriptHashes
      ]
    , testGroup "compiled"
      [ testCase "compile scripts" (assertBool "should be right" $ isRight Scripts.compileScripts)
      , testCase "expected script hashes" scriptHashesCompiled
      , testCase "expected script hashes (minting)" scriptHashesMinting
      ]
    ]
  ]

scriptHashes :: Assertion
scriptHashes = do
  Right s <- runExceptT Scripts.loadScripts
  checkHashes s

scriptHashesCompiled :: Assertion
scriptHashesCompiled = either (fail . show) checkHashes Scripts.compileScripts

scriptHashesMinting :: Assertion
scriptHashesMinting = do
  case Scripts.poolNftMintingScript txOutRef tokenName of
    Left err -> fail (show err)
    Right script -> assertEqual "poolNftMintingScript" "de648b8b745f12ea61b73e792fb27c6a5c531bb467cd7f6d956bac2c" (C.hashScript script)
  case Scripts.poolLqMintingScript txOutRef tokenName 10 of
    Left err -> fail (show err)
    Right script -> assertEqual "poolLqMintingScript" "8d6b18773188098fcbc4d0a5a1eb3850f794f5f3b72bf7472efc6a72" (C.hashScript script)

txOutRef :: TxOutRef
txOutRef = TxOutRef "de98d293a887f4e58634d629a58b1d12344bc2541403177b0e4eedafc08971c1" 0

tokenName :: TokenName
tokenName = "token"

checkHashes :: Scripts -> Assertion
checkHashes Scripts{sPoolValidator, sSwapValidator, sDepositValidator, sRedeemValidator} = do
  assertEqual "Pool validator"    "4888c78356d18d2858be5729544626966676b57312d4355b0f4f9535" (C.hashScript sPoolValidator)
  assertEqual "Deposit validator" "93ee3a2a7059146ff9231068b55c84ad6884274e8f8106e36369f5f2" (C.hashScript sDepositValidator)
  assertEqual "Swap validator"    "dc7c2eec5a87a4d972d0405187eec782ae80bea72492cd7b244c3870" (C.hashScript sSwapValidator)
  assertEqual "Redeem validator"  "aeac18c3ffc9e94109c173caa0500015aea23c45773b478754699cee" (C.hashScript sRedeemValidator)
