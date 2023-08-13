{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Building teddyswap transactions
-}
module Teddy.Matcher.BuildTx(
  PoolNFT(..),
  createPoolNft,
  PoolLiquidityToken(..),
  createPoolLiquidityToken
) where

import           Cardano.Api           (AssetName, PolicyId, TxIn)
import qualified Cardano.Api           as C
import           Convex.BuildTx        (TxBuild, mintPlutusV2,
                                        spendPublicKeyOutput)
import           Convex.PlutusLedger   (transTxOutRef)
import           ErgoDex.CardanoApi    (CardanoApiScriptError)
import           Teddy.Matcher.Scripts (poolLqMintingScript,
                                        poolNftMintingScript)

data PoolNFT =
  PoolNFT
    { pnftTxBuild :: TxBuild
    , pnftAsset   :: (PolicyId, AssetName)
    }

createPoolNft :: TxIn -> Either CardanoApiScriptError PoolNFT
createPoolNft txInput = do
  let tn = "pool nft"
  script <- poolNftMintingScript (transTxOutRef txInput) "pool nft" >>= \case
    C.PlutusScript C.PlutusScriptV2  s -> pure s
  let scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
  pure PoolNFT
    { pnftTxBuild = mintPlutusV2 script () tn 1 . spendPublicKeyOutput txInput
    , pnftAsset = (C.PolicyId scriptHash, tn)
    }

data PoolLiquidityToken =
  PoolLiquidityToken
    { pltTxBuild :: TxBuild
    , pltAsset   :: (PolicyId, AssetName)
    }

createPoolLiquidityToken :: TxIn -> Integer -> Either CardanoApiScriptError PoolLiquidityToken
createPoolLiquidityToken txInput n = do
  let tn = "liquidity"
  script <- poolLqMintingScript (transTxOutRef txInput) "liquidity" n >>= \case
    C.PlutusScript C.PlutusScriptV2  s -> pure s
  let scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
  pure PoolLiquidityToken
    { pltTxBuild = mintPlutusV2 script () tn 1 . spendPublicKeyOutput txInput
    , pltAsset = (C.PolicyId scriptHash, tn)
    }
