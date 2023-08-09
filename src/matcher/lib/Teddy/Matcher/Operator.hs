{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Managing the credentials used for batching |-}
module Teddy.Matcher.Operator(
  -- * Operator
  PaymentExtendedKey(..),
  Signing,
  Verification,
  toVerification,
  verificationKey,
  signTx,
  Operator(..),
  operatorAddress,
  operatorPaymentCredential,
  operatorUtxos,
  operatorWalletID,
  balanceAndSubmitOperator,
  selectOperatorUTxO,
  -- * Error handling
  BalanceAndSubmitError(..),

) where

import qualified Cardano.Api               as C
import           Control.Monad.Error.Class (MonadError (..))
import           Convex.Class              (MonadBlockchain, sendTx)
import           Convex.PlutusLedger       (transPubKeyHash, transStakeKeyHash)
import           Convex.Wallet             (addSignature, addSignatureExtended)
import qualified Data.Map                  as Map
import           Data.Maybe                (listToMaybe)
import           PlutusLedgerApi.V1        (PubKeyHash (..))
import           Teddy.Matcher.Query       (BalancingError, MonadUtxoQuery (..),
                                            balanceTx)
import           Teddy.Matcher.Utils       (liftEither, liftResult)

data Signing

data Verification

data PaymentExtendedKey k where
  PESigning :: C.SigningKey C.PaymentKey -> PaymentExtendedKey Signing
  PESigningEx :: C.SigningKey C.PaymentExtendedKey -> PaymentExtendedKey Signing
  PEVerification :: C.VerificationKey C.PaymentKey -> PaymentExtendedKey Verification

deriving stock instance Show (PaymentExtendedKey k)

verificationKey :: PaymentExtendedKey k -> C.VerificationKey C.PaymentKey
verificationKey = \case
  PESigningEx k    -> C.castVerificationKey $ C.getVerificationKey k
  PESigning k      -> C.getVerificationKey k
  PEVerification k -> k

toVerification :: PaymentExtendedKey Signing -> PaymentExtendedKey Verification
toVerification = PEVerification . verificationKey

signTx :: C.IsShelleyBasedEra era => PaymentExtendedKey Signing -> C.Tx era -> C.Tx era
signTx = \case
  PESigningEx k -> addSignatureExtended k
  PESigning   k -> addSignature k

{-| An entity that can match orders
-}
data Operator k =
  Operator
    { oPaymentKey :: PaymentExtendedKey k
    , oStakeKey   :: Maybe (C.VerificationKey C.StakeKey)
    }

deriving stock instance Show (PaymentExtendedKey k) => Show (Operator k)

instance Show (PaymentExtendedKey k) => Eq (Operator k) where
  l == r = show l == show r

{-| Address of the operator in a network
-}
operatorAddress :: C.NetworkId -> Operator k -> C.Address C.ShelleyAddr
operatorAddress networkId op =
  C.makeShelleyAddress
    networkId
    (operatorPaymentCredential op)
    C.NoStakeAddress

{-| The operator's payment credential (public key)
-}
operatorPaymentCredential :: Operator k -> C.PaymentCredential
operatorPaymentCredential = C.PaymentCredentialByKey . C.verificationKeyHash . verificationKey . oPaymentKey

{-| Key hashes in Plutus format
-}
operatorWalletID :: Operator k -> (PubKeyHash, Maybe PubKeyHash)
operatorWalletID Operator{oPaymentKey, oStakeKey} =
  ( transPubKeyHash $ C.verificationKeyHash $ verificationKey oPaymentKey
  , fmap (transStakeKeyHash . C.verificationKeyHash) oStakeKey
  )

{-| UTxOs that are locked by the operator's payment credential
|-}
operatorUtxos :: MonadUtxoQuery m => Operator k -> m (C.UTxO C.BabbageEra)
operatorUtxos = utxosByPayment . operatorPaymentCredential

{-| Select a single UTxO that is controlled by the operator. |-}
selectOperatorUTxO :: MonadUtxoQuery m => Operator k -> m (Maybe (C.TxIn, C.TxOut C.CtxUTxO C.BabbageEra))
selectOperatorUTxO operator = fmap (listToMaybe . Map.toList . C.unUTxO) (operatorUtxos operator)

-- | Balance a transaction body, sign it with the operator's key, and submit it to the network.
balanceAndSubmitOperator :: (MonadBlockchain m, MonadUtxoQuery m, MonadError BalanceAndSubmitError m) => Operator Signing -> C.TxBodyContent C.BuildTx C.BabbageEra -> m (C.Tx C.BabbageEra)
balanceAndSubmitOperator op@Operator{oPaymentKey} txBody = do
  (C.BalancedTxBody _ balancedTxBody _changeOutput _fee, _) <- liftEither BalanceError $
    balanceTx (operatorPaymentCredential op) Nothing txBody
  let finalTx = signTx oPaymentKey $ C.makeSignedTransaction [] balancedTxBody
  liftResult SubmitError (sendTx finalTx) >> pure finalTx

-- | Failures during txn balancing and submission
data BalanceAndSubmitError =
  BalanceError BalancingError
  | SubmitError String
  deriving Show
