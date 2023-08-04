{-# LANGUAGE TypeApplications #-}
module Teddy.Matcher.Scripts(
  Scripts(..),
  loadScripts
) where

import qualified Cardano.Api           as C
import qualified Data.ByteString.Short as Short
import           Data.Proxy            (Proxy (..))
import qualified ErgoDex.PValidators   as V

loadScripts :: IO Scripts
loadScripts =
  Scripts
    <$> fmap (either (error . show) id . C.deserialiseFromCBOR (C.proxyToAsType $ Proxy @(C.Script C.PlutusScriptV2)) . Short.fromShort) V.poolValidator

-- | Plutus scripts that we need for the dex
data Scripts =
  Scripts
    { sPoolValidator :: C.Script C.PlutusScriptV2
    }
    deriving Show
