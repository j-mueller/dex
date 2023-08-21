{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-| Building teddyswap transactions
-}
module Teddy.Matcher.BuildTx(
  DEXBuildTxError(..),

  -- * DEX actions
  PoolNFT(..),
  createPoolNft,
  PoolLiquidityToken(..),
  createPoolLiquidityToken,
  PoolOutput(..),
  addPoolOutput,

  -- * Deposits
  Deposit(..),
  deposit,

  poolConfig
) where

import           Cardano.Api                     (AssetName, PolicyId,
                                                  Quantity (..), TxIn, Value)
import qualified Cardano.Api.Shelley             as C
import           Control.Lens                    (over)
import           Control.Monad.Except            (MonadError, liftEither,
                                                  throwError)
import           Convex.BuildTx                  (MonadBuildTx (..),
                                                  minAdaDeposit, mintPlutusV2,
                                                  spendPublicKeyOutput)
import qualified Convex.Lenses                   as L
import           Convex.PlutusLedger             (transAssetId, transPubKeyHash,
                                                  transStakeKeyHash,
                                                  transTxOutRef, unTransAssetId)
import           Convex.Scripts                  (toHashableScriptData)
import           ErgoDex.CardanoApi              (CardanoApiScriptError,
                                                  depositScript,
                                                  poolLqMintingScript,
                                                  poolNftMintingScript,
                                                  poolScript)
import           ErgoDex.Contracts.Pool          (PoolConfig (..))
import           ErgoDex.Contracts.Proxy.Deposit (DepositConfig)
import qualified ErgoDex.Contracts.Proxy.Deposit as D
import           PlutusLedgerApi.V1.Value        (AssetClass)
import           Teddy.Matcher.Utils             (mapError)

data DEXBuildTxError =
  BuildTxScriptError CardanoApiScriptError
  | CardanoApiError C.SerialiseAsRawBytesError
  deriving Show

data PoolNFT =
  PoolNFT
    { pnftAsset   :: (PolicyId, AssetName)
    }

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

data Deposit =
  Deposit
    { doTxOut         :: C.TxOut C.CtxTx C.BabbageEra
    , doDepositConfig :: DepositConfig
    }

deposit :: (MonadBuildTx m, MonadError DEXBuildTxError m) => C.NetworkId -> C.BundledProtocolParameters C.BabbageEra -> C.Hash C.PaymentKey -> Maybe (C.Hash C.StakeKey) -> PoolConfig -> (Quantity, Quantity) -> m Deposit
deposit networkId protParams verificationKey stakeKey cfg@PoolConfig{poolNft, poolX, poolY, poolLq} quantities = do
  value <- mapError CardanoApiError (depositValue cfg quantities)
  let depositCfg0 =
        D.DepositConfig
          { D.poolNft = poolNft
          , D.tokenA = poolX
          , D.tokenB = poolY
          , D.tokenLp = poolLq
          , D.exFee = 1 -- FIXME calculate fee
          , D.rewardPkh = transPubKeyHash verificationKey
          , D.stakePkh = transStakeKeyHash <$> stakeKey
          , D.collateralAda = 2_000_000
          }
  output0 <- mapError BuildTxScriptError (depositTxOut networkId depositCfg0 value)
  let Quantity minAda = minAdaDeposit protParams output0
  let depositCfg1 = depositCfg0{D.collateralAda = minAda}
  output1 <- mapError BuildTxScriptError (depositTxOut networkId depositCfg1 (value <> C.lovelaceToValue (C.Lovelace minAda)))
  addBtx $ over L.txOuts ((:) output1)
  pure $ Deposit output1 depositCfg1

{-| The @cardano-api@ value containing the tokens to be deposited to a liquidity pool.
-}
depositValue :: MonadError C.SerialiseAsRawBytesError m => PoolConfig -> (Quantity, Quantity) -> m Value
depositValue PoolConfig{poolX, poolY} (amountX, amountY) = do
  aX <- liftEither (unTransAssetId poolX)
  aY <- liftEither (unTransAssetId poolY)
  pure $ C.valueFromList [(aX, amountX), (aY, amountY)]

{-| The tx output for a token deposit
-}
depositTxOut :: MonadError CardanoApiScriptError m => C.NetworkId -> DepositConfig -> Value -> m (C.TxOut C.CtxTx C.BabbageEra)
depositTxOut networkId cfg value = do
  script <- liftEither depositScript
  let addr = C.makeShelleyAddressInEra networkId (C.PaymentCredentialByScript (C.hashScript script)) C.NoStakeAddress
      dat = C.TxOutDatumInTx C.ScriptDataInBabbageEra (toHashableScriptData cfg)
  pure (C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra value) dat C.ReferenceScriptNone)
