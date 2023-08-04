{-# LANGUAGE TypeApplications #-}
module Teddy.Matcher.Scripts(
  Scripts(..),
  loadScripts
) where

import qualified Cardano.Api           as C
import           Cardano.Binary        (DecoderError)
import qualified Data.ByteString.Short as Short
import           Data.Proxy            (Proxy (..))
import qualified ErgoDex.PValidators   as V

loadScripts :: IO (Either DecoderError Scripts)
loadScripts =
  Scripts
    <$> V.poolValidator
    <*> V.swapValidator
    <*> V.depositValidator
    <*> V.redeemValidator

-- | Plutus scripts that we need for the dex
data Scripts =
  Scripts
    { sPoolValidator    :: C.Script C.PlutusScriptV2
    , sSwapValidator    :: C.Script C.PlutusScriptV2
    , sDepositValidator :: C.Script C.PlutusScriptV2
    , sRedeemValidator  :: C.Script C.PlutusScriptV2
    }
    deriving Show
