{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Building teddyswap transactions
-}
module Teddy.Matcher.BuildTx(
  -- * Build pool transactions
  BuildPoolTx,
  TXB(..),
  runBuildPoolTx,

  -- * DEX actions
  PoolNFT(..),
  createPoolNft,
  PoolLiquidityToken(..),
  createPoolLiquidityToken,
  PoolOutput(..),
  addPoolOutput
) where

import           Cardano.Api            (AssetName, PolicyId, TxIn, Value)
import qualified Cardano.Api.Shelley    as C
import           Control.Lens           (over)
import           Control.Monad.Except   (ExceptT, MonadError, liftEither,
                                         runExceptT, throwError)
import           Control.Monad.Writer   (MonadWriter (tell), WriterT,
                                         runWriterT)
import           Convex.BuildTx         (TxBuild, mintPlutusV2,
                                         spendPublicKeyOutput)
import qualified Convex.Lenses          as L
import           Convex.PlutusLedger    (transTxOutRef)
import           Convex.Scripts         (toHashableScriptData)
import           Data.Monoid            (Endo (..))
import           ErgoDex.CardanoApi     (CardanoApiScriptError,
                                         poolLqMintingScript,
                                         poolNftMintingScript, poolScript)
import           ErgoDex.Contracts.Pool (PoolConfig (..))

data PoolNFT =
  PoolNFT
    { pnftAsset   :: (PolicyId, AssetName)
    }

type BuildPoolTx m a = WriterT TXB (ExceptT CardanoApiScriptError m) a

newtype TXB = TXB{ getTxB :: TxBuild }
  deriving (Semigroup, Monoid) via (Endo (C.TxBodyContent C.BuildTx C.BabbageEra))

runBuildPoolTx :: Functor m => BuildPoolTx m a -> m (Either CardanoApiScriptError (a, TxBuild))
runBuildPoolTx = fmap (fmap (fmap getTxB)) . runExceptT . runWriterT

createPoolNft :: (MonadWriter TXB m, MonadError CardanoApiScriptError m) => TxIn -> m PoolNFT
createPoolNft txInput = do
  let tn = "pool nft"
  script <- case poolNftMintingScript (transTxOutRef txInput) "pool nft" of
    Right (C.PlutusScript C.PlutusScriptV2  s) -> pure s
    Left err                                   -> throwError err
  let scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
  tell $ TXB $ mintPlutusV2 script () tn 1 . spendPublicKeyOutput txInput
  pure PoolNFT
    { pnftAsset = (C.PolicyId scriptHash, tn)
    }

data PoolLiquidityToken =
  PoolLiquidityToken
    { pltAsset   :: (PolicyId, AssetName)
    }

createPoolLiquidityToken :: (MonadWriter TXB m, MonadError CardanoApiScriptError m) => TxIn -> Integer -> m PoolLiquidityToken
createPoolLiquidityToken txInput n = do
  let tn = "liquidity"
  script <- case poolLqMintingScript (transTxOutRef txInput) "liquidity" n of
    Right (C.PlutusScript C.PlutusScriptV2 s) -> pure s
    Left x                                    -> throwError x
  let scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
  tell $ TXB $ mintPlutusV2 script () tn 1 . spendPublicKeyOutput txInput
  pure PoolLiquidityToken
    { pltAsset = (C.PolicyId scriptHash, tn)
    }

data PoolOutput =
  PoolOutput
    { poTxOut  :: (C.TxOut C.CtxTx C.BabbageEra)
    , poConfig :: PoolConfig
    , poValue  :: C.Value
    }

-- | Add the pool output to a transaction
addPoolOutput :: (MonadWriter TXB m, MonadError CardanoApiScriptError m) => C.NetworkId -> PoolConfig -> Value -> m PoolOutput
addPoolOutput networkId cfg@PoolConfig{} value = do
  script <- liftEither poolScript

  let addr = C.makeShelleyAddressInEra networkId (C.PaymentCredentialByScript (C.hashScript script)) C.NoStakeAddress
      dat = C.TxOutDatumInTx C.ScriptDataInBabbageEra (toHashableScriptData cfg)
  let txOut = C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra value) dat C.ReferenceScriptNone -- TODO: Use a ref. script?
  tell $ TXB $ over L.txOuts ((:) txOut)
  pure PoolOutput
    { poTxOut = txOut
    , poConfig = cfg
    , poValue = value
    }
