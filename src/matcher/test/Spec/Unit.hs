{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Spec.Unit(
  tests
) where

import           Cardano.Api            (AssetName, PolicyId)
import qualified Cardano.Api            as C
import           Control.Monad.Except   (runExceptT)
import           Convex.Class           (MonadBlockchain)
import           Convex.Lenses          (emptyTx)
import           Convex.MockChain.Utils (mockchainSucceeds)
import           Data.Aeson             (Result (..), fromJSON, object, (.=))
import           Data.Bifunctor         (Bifunctor (..))
import           Data.Function          ((&))
import           Data.Proxy             (Proxy (..))
import qualified Teddy.Matcher.BuildTx  as BuildTx
import           Teddy.Matcher.BuildTx  (PoolLiquidityToken (..), PoolNFT (..))
import           Teddy.Matcher.Operator (Operator (..), PaymentExtendedKey (..),
                                         Signing, balanceAndSubmitOperator,
                                         selectOperatorUTxO)
import           Teddy.Matcher.Query    (MonadUtxoQuery)
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (testCase)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "create LQ pool NFT" (mockchainSucceeds createLQPoolNft)
  , testCase "create LQ pool liquidity" (mockchainSucceeds createLQPoolLiquidity)
  -- , testCase "create LQ pool" (mockchainSucceeds createLQPool)
  ]

createLQPoolNft :: (MonadUtxoQuery m, MonadFail m, MonadBlockchain m) => m (C.Tx C.BabbageEra, (PolicyId, AssetName))
createLQPoolNft = do
  utxo <- selectOperatorUTxO testOperator >>= maybe (fail "No UTxO found") pure
  PoolNFT{pnftTxBuild, pnftAsset} <- either (\x -> fail $ "BuildTx failed: " <> show x) pure (BuildTx.createPoolNft (fst utxo))
  runExceptT (balanceAndSubmitOperator testOperator (pnftTxBuild emptyTx)) >>= either (fail . show) (pure . (,pnftAsset))

createLQPoolLiquidity :: (MonadUtxoQuery m, MonadFail m, MonadBlockchain m) => m (C.Tx C.BabbageEra, (PolicyId, AssetName))
createLQPoolLiquidity = do
  utxo <- selectOperatorUTxO testOperator >>= maybe (fail "No UTxO found") pure
  PoolLiquidityToken{pltTxBuild, pltAsset} <- either (\x -> fail $ "BuildTx failed: " <> show x) pure (BuildTx.createPoolLiquidityToken (fst utxo) 10000)
  runExceptT (balanceAndSubmitOperator testOperator (pltTxBuild emptyTx)) >>= either (fail . show) (pure . (,pltAsset))

testOperator :: Operator Signing
testOperator =
  let oPaymentKey =
        either (error . (<>) "expected key. " . show) PESigning
        $ signingKeyFromCbor "5880b808c3a5df79e6b9130d15f11fbdc019246448250adb2fec1c2d0aaf0cca154aeee499350c8767aeed3d7043b9719a112f5a1cc594c2debcbc7ec779eb8b89ee049dbb91c5b3804f45551566c72a544758eea407c19f944de6b4c8b33678d1e8bda691ec03966b364650bf78ebb1a7d9011380cdcaf70446bf844dbe423dcd50"
  in Operator
      { oPaymentKey
      , oStakeKey = Nothing
      }

signingKeyFromCbor :: String -> Either String (C.SigningKey C.PaymentKey)
signingKeyFromCbor cbor = do
  let s :: String -> String
      s = id
      vl = object ["type" .= s "PaymentExtendedSigningKeyShelley_ed25519_bip32", "description" .= s "", "cborHex" .= cbor]
  textEnvelope <- fromJSON vl & (\case { Error err -> Left (show err); Success e -> Right e })
  C.deserialiseFromTextEnvelope (C.proxyToAsType Proxy) textEnvelope & first show
