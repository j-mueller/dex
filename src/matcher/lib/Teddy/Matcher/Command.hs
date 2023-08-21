module Teddy.Matcher.Command (
  ActivePool(..),
  CommandError(..),
  CreatePoolParams(..),
  createPool
) where

import           Cardano.Api            (AssetId)
import qualified Cardano.Api            as C
import           Control.Monad.Except   (MonadError, throwError)
import           Convex.BuildTx         (setMinAdaDepositAll)
import           Convex.Class           (MonadBlockchain (..))
import           Convex.Lenses          (emptyTx)
import qualified Convex.PlutusLedger    as PL
import           Convex.Utils           (txnUtxos)
import           ErgoDex.CardanoApi     (CardanoApiScriptError)
import           ErgoDex.Contracts.Pool (PoolConfig (..))
import qualified Teddy.Matcher.BuildTx  as BuildTx
import           Teddy.Matcher.BuildTx  (runBuildPoolTx)
import           Teddy.Matcher.Operator (BalanceAndSubmitError, Operator,
                                         Signing, balanceAndSubmitOperator,
                                         selectOperatorUTxO)
import           Teddy.Matcher.Query    (MonadUtxoQuery)
import           Teddy.Matcher.Utils    (liftEither, mapError)

data CommandError =
  NoSuitableInputFound (Operator Signing)
  | ScriptError CardanoApiScriptError
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
    , cppAssetClassX  :: AssetId
    , cppAssetClassY  :: AssetId
    }

{-| Create a new LP pool with a number of liquidity tokens.
-}
createPool :: (MonadUtxoQuery m, MonadBlockchain m, MonadError CommandError m) => CreatePoolParams -> m ActivePool
createPool CreatePoolParams{cppOperator, cppNumLiqTokens, cppFee, cppAssetClassX, cppAssetClassY} = do
  (txi, _) <- selectOperatorUTxO cppOperator >>= maybe (throwError $ NoSuitableInputFound cppOperator) pure
  (apPoolConfig, btx) <- liftEither ScriptError $ runBuildPoolTx $ do
    cfg <- BuildTx.poolConfig (PL.transAssetId cppAssetClassX) (PL.transAssetId cppAssetClassY) cppFee
            <$> BuildTx.createPoolLiquidityToken txi cppNumLiqTokens
            <*> BuildTx.createPoolNft txi
    n <- networkId
    _ <- BuildTx.addPoolOutput n cfg mempty
    queryProtocolParameters >>= setMinAdaDepositAll
    pure cfg
  apTx <- mapError BalanceSubmitFailed (balanceAndSubmitOperator cppOperator (btx emptyTx))
  pure ActivePool{apTx, apPoolConfig, apTxOut = head (txnUtxos apTx)}
