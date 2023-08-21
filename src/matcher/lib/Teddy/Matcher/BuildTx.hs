{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Building teddyswap transactions
-}
module Teddy.Matcher.BuildTx(
  -- * Build pool transactions
  BuildPoolTx,
  runBuildPoolTx,

  -- * DEX actions
  PoolNFT(..),
  createPoolNft,
  PoolLiquidityToken(..),
  createPoolLiquidityToken,
  PoolOutput(..),
  addPoolOutput,

  poolConfig
) where

import           Cardano.Api              (AssetName, PolicyId, TxIn, Value)
import qualified Cardano.Api.Shelley      as C
import           Control.Lens             (over)
import           Control.Monad.Except     (ExceptT, MonadError, liftEither,
                                           runExceptT, throwError)
import           Convex.BuildTx           (BuildTxT, MonadBuildTx (..), TxBuild,
                                           mintPlutusV2, runBuildTxT,
                                           spendPublicKeyOutput)
import qualified Convex.Lenses            as L
import           Convex.PlutusLedger      (transAssetId, transTxOutRef)
import           Convex.Scripts           (toHashableScriptData)
import           ErgoDex.CardanoApi       (CardanoApiScriptError,
                                           poolLqMintingScript,
                                           poolNftMintingScript, poolScript)
import           ErgoDex.Contracts.Pool   (PoolConfig (..))
import           PlutusLedgerApi.V1.Value (AssetClass)

data PoolNFT =
  PoolNFT
    { pnftAsset   :: (PolicyId, AssetName)
    }

type BuildPoolTx m a = BuildTxT (ExceptT CardanoApiScriptError m) a

runBuildPoolTx :: Functor m => BuildPoolTx m a -> m (Either CardanoApiScriptError (a, TxBuild))
runBuildPoolTx = runExceptT . runBuildTxT

createPoolNft :: (MonadBuildTx m, MonadError CardanoApiScriptError m) => TxIn -> m PoolNFT
createPoolNft txInput = do
  let tn = "pool nft"
  script <- case poolNftMintingScript (transTxOutRef txInput) "pool nft" of
    Right (C.PlutusScript C.PlutusScriptV2  s) -> pure s
    Left err                                   -> throwError err
  let scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
  mintPlutusV2 script () tn 1
  spendPublicKeyOutput txInput
  pure PoolNFT
    { pnftAsset = (C.PolicyId scriptHash, tn)
    }

data PoolLiquidityToken =
  PoolLiquidityToken
    { pltAsset   :: (PolicyId, AssetName)
    }

createPoolLiquidityToken :: (MonadBuildTx m, MonadError CardanoApiScriptError m) => TxIn -> Integer -> m PoolLiquidityToken
createPoolLiquidityToken txInput n = do
  let tn = "liquidity"
  script <- case poolLqMintingScript (transTxOutRef txInput) "liquidity" n of
    Right (C.PlutusScript C.PlutusScriptV2 s) -> pure s
    Left x                                    -> throwError x
  let scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
  mintPlutusV2 script () tn 1
  spendPublicKeyOutput txInput
  pure PoolLiquidityToken
    { pltAsset = (C.PolicyId scriptHash, tn)
    }

poolConfig :: AssetClass -> AssetClass -> Integer -> PoolLiquidityToken -> PoolNFT -> PoolConfig
poolConfig poolX poolY poolFeeNum  PoolLiquidityToken{pltAsset} PoolNFT{pnftAsset} =
  PoolConfig
    { poolNft = transAssetId $ uncurry C.AssetId pnftAsset
    , poolX
    , poolY
    , poolLq = transAssetId $ uncurry C.AssetId pltAsset
    , poolFeeNum
    }

data PoolOutput =
  PoolOutput
    { poTxOut  :: (C.TxOut C.CtxTx C.BabbageEra)
    , poConfig :: PoolConfig
    , poValue  :: C.Value
    }

-- | Add the pool output to a transaction
addPoolOutput :: (MonadBuildTx m, MonadError CardanoApiScriptError m) => C.NetworkId -> PoolConfig -> Value -> m PoolOutput
addPoolOutput networkId cfg@PoolConfig{} value = do
  script <- liftEither poolScript

  let addr = C.makeShelleyAddressInEra networkId (C.PaymentCredentialByScript (C.hashScript script)) C.NoStakeAddress
      dat = C.TxOutDatumInTx C.ScriptDataInBabbageEra (toHashableScriptData cfg)
  let txOut = C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra value) dat C.ReferenceScriptNone -- TODO: Use a ref. script?
  addBtx $ over L.txOuts ((:) txOut)
  pure PoolOutput
    { poTxOut = txOut
    , poConfig = cfg
    , poValue = value
    }
