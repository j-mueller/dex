{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Custom class to encapsulate the general purpose
queries that we need for building transactions
-}
module Teddy.Matcher.Query(
  MonadUtxoQuery(..),
  BalancingError(..),
  balanceTx,

) where

import           Cardano.Api                       (BabbageEra, BalancedTxBody,
                                                    BuildTx,
                                                    PaymentCredential (..),
                                                    StakeAddressReference (..),
                                                    StakeCredential,
                                                    TxBodyContent, UTxO (..),
                                                    makeShelleyAddressInEra)
import           Control.Monad.Trans.Class         (MonadTrans (..))
import           Control.Monad.Trans.Except        (ExceptT, runExceptT)
import           Control.Monad.Trans.Except.Result (ResultT)
import           Control.Monad.Trans.Reader        (ReaderT)
import qualified Control.Monad.Trans.State         as StrictState
import qualified Control.Monad.Trans.State.Strict  as LazyState
import           Convex.Class                      (MonadBlockchain (..),
                                                    MonadBlockchainCardanoNodeT,
                                                    networkId)
import qualified Convex.CoinSelection
import           Convex.MockChain                  (MockchainT, utxoSet)
import           Convex.MonadLog                   (MonadLogIgnoreT)
import           Convex.Utxos                      (BalanceChanges, fromApiUtxo,
                                                    onlyCredential, toApiUtxo)
import           Teddy.Matcher.Utils               (liftResult)

class Monad m => MonadUtxoQuery m where
  utxosByPayment :: PaymentCredential -> m (UTxO BabbageEra)

instance Monad m => MonadUtxoQuery (MockchainT m) where
  utxosByPayment cred = toApiUtxo . onlyCredential cred <$> utxoSet

instance MonadUtxoQuery m => MonadUtxoQuery (ResultT m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (ExceptT e m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (ReaderT e m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (StrictState.StateT s m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (LazyState.StateT s m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (MonadBlockchainCardanoNodeT e m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (MonadLogIgnoreT m) where
  utxosByPayment = lift . utxosByPayment

newtype BalancingError = BalancingError String
  deriving stock (Eq, Ord, Show)

{-| Balance the transaction body using the UTxOs locked by the payment credential,
and returning any unused funds to the given payment credential and stake credential
|-}
balanceTx :: (MonadBlockchain m, MonadUtxoQuery m) => PaymentCredential -> Maybe StakeCredential -> TxBodyContent BuildTx BabbageEra -> m (Either BalancingError (BalancedTxBody BabbageEra, BalanceChanges))
balanceTx operator stakeCred txBody = do
  n <- networkId
  o <- fromApiUtxo <$> utxosByPayment operator
  let returnAddress = makeShelleyAddressInEra n operator (maybe NoStakeAddress StakeAddressByValue stakeCred)
  runExceptT $ liftResult BalancingError (Convex.CoinSelection.balanceTx returnAddress o txBody)
