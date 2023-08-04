{-# LANGUAGE TypeApplications #-}
module Teddy.Matcher.Scripts(
  Scripts(..),
  loadScripts
) where

import qualified Cardano.Api          as C
import           Cardano.Binary       (DecoderError)
import           Control.Monad.Except (ExceptT (..))
import qualified ErgoDex.PValidators  as V

loadScripts :: ExceptT DecoderError IO Scripts
loadScripts =
  Scripts
    <$> ExceptT V.poolValidator
    <*> ExceptT V.swapValidator
    <*> ExceptT V.depositValidator
    <*> ExceptT V.redeemValidator

-- | Plutus scripts that we need for the dex
data Scripts =
  Scripts
    { sPoolValidator    :: C.Script C.PlutusScriptV2
    , sSwapValidator    :: C.Script C.PlutusScriptV2
    , sDepositValidator :: C.Script C.PlutusScriptV2
    , sRedeemValidator  :: C.Script C.PlutusScriptV2
    }
    deriving Show
