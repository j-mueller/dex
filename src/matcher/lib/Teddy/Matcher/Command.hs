module Teddy.Matcher.Command (
  ActivePool(..),
  CommandError(..),

  -- * Creating LQ Pools
  CreatePoolParams(..),
  createPool,

  -- * Making deposits
  DepositOutput(..),
  MakeDepositParams(..),
  makeDeposit
) where

import           Cardano.Api            (AssetId, Quantity, Value)
import qualified Cardano.Api            as C
import           Control.Monad.Except   (MonadError, throwError)
import           Convex.BuildTx         (runBuildTxT, setMinAdaDepositAll)
import           Convex.Class           (MonadBlockchain (..))
import           Convex.Lenses          (emptyTx)
import qualified Convex.PlutusLedger    as PL
import           Convex.Utils           (txnUtxos)
import           ErgoDex.Contracts.Pool (PoolConfig (..))
import qualified Teddy.Matcher.BuildTx  as BuildTx
import           Teddy.Matcher.BuildTx  (DEXBuildTxError (..), Deposit)
import           Teddy.Matcher.Operator (BalanceAndSubmitError, Operator (..),
                                         Signing, balanceAndSubmitOperator,
                                         selectOperatorUTxO, verificationKey)
import           Teddy.Matcher.Query    (MonadUtxoQuery)
import           Teddy.Matcher.Utils    (mapError)

data CommandError =
  NoSuitableInputFound (Operator Signing)
  | BuildTxError DEXBuildTxError
  | BalanceSubmitFailed BalanceAndSubmitError
  deriving stock (Show)

-- | An active liquidity pool
data ActivePool =
  ActivePool
    { apPoolConfig :: PoolConfig
    , apTx         :: C.Tx C.BabbageEra
    , apTxOut      :: (C.TxIn, C.TxOut C.CtxTx C.BabbageEra)
    }

data CreatePoolParams =
  CreatePoolParams
    { cppOperator     :: Operator Signing
    , cppNumLiqTokens :: Integer
    , cppFee          :: Integer -- ^ Fee in 1/1000th of the traded amount
    , cppAssetClassX  :: (AssetId, Quantity)
    , cppAssetClassY  :: (AssetId, Quantity)
    }

initialValue :: CreatePoolParams -> Value
initialValue CreatePoolParams{cppAssetClassX, cppAssetClassY} =
  C.valueFromList [cppAssetClassX, cppAssetClassY]

{-| Create a new LP pool with a number of liquidity tokens.
-}
createPool :: (MonadUtxoQuery m, MonadBlockchain m, MonadError CommandError m) => CreatePoolParams -> m ActivePool
createPool params@CreatePoolParams{cppOperator, cppNumLiqTokens, cppFee, cppAssetClassX, cppAssetClassY} = do
  (txi, _) <- selectOperatorUTxO cppOperator >>= maybe (throwError $ NoSuitableInputFound cppOperator) pure
  (apPoolConfig, btx) <- runBuildTxT $ mapError (BuildTxError . BuildTxScriptError) $ do
    cfg <- BuildTx.poolConfig (PL.transAssetId $ fst cppAssetClassX) (PL.transAssetId $ fst cppAssetClassY) cppFee
            <$> BuildTx.createPoolLiquidityToken txi cppNumLiqTokens
            <*> BuildTx.createPoolNft txi
    n <- networkId
    _ <- BuildTx.addPoolOutput n cfg (initialValue params)
    queryProtocolParameters >>= setMinAdaDepositAll
    pure cfg
  apTx <- mapError BalanceSubmitFailed (balanceAndSubmitOperator cppOperator (btx emptyTx))
  pure ActivePool{apTx, apPoolConfig, apTxOut = head (txnUtxos apTx)}

{-| Parameters for a deposit to a liquidity pool
-}
data MakeDepositParams =
  MakeDepositParams
    { mdpOperator   :: Operator Signing
    , mdpPoolConfig :: PoolConfig
    , mdpQuantities :: (Quantity, Quantity)
    }

-- | UTxO with a deposit
data DepositOutput =
  DepositOutput
    { doDeposit :: Deposit
    , doTx      :: C.Tx C.BabbageEra
    , doTxOut   :: C.TxIn
    }

makeDeposit :: (MonadUtxoQuery m, MonadBlockchain m, MonadError CommandError m) => MakeDepositParams -> m DepositOutput
makeDeposit MakeDepositParams{mdpOperator, mdpPoolConfig, mdpQuantities} = do
  (doDeposit, btx) <- runBuildTxT $ mapError BuildTxError $ do
    (n, pParams) <- (,) <$> networkId <*> queryProtocolParameters
    BuildTx.deposit n pParams (C.verificationKeyHash $ verificationKey $ oPaymentKey mdpOperator) (C.verificationKeyHash <$> oStakeKey  mdpOperator) mdpPoolConfig mdpQuantities
  doTx <- mapError BalanceSubmitFailed (balanceAndSubmitOperator mdpOperator (btx emptyTx))
  pure DepositOutput{doTx, doDeposit, doTxOut = fst $ head (txnUtxos doTx)}
