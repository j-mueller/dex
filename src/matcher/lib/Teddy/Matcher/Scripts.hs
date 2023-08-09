{-# LANGUAGE TypeApplications #-}
module Teddy.Matcher.Scripts(
  Scripts(..),
  loadScripts,
  compileScripts,

  poolNftMintingScript,
  poolLqMintingScript,
  CardanoApiScriptError(..)
) where

import qualified Cardano.Api          as C
import           Cardano.Binary       (DecoderError)
import           Control.Monad.Except (ExceptT (..))
import           ErgoDex.CardanoApi   (CardanoApiScriptError,
                                       poolLqMintingScript,
                                       poolNftMintingScript)
import qualified ErgoDex.CardanoApi   as E
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

compileScripts :: Either CardanoApiScriptError Scripts
compileScripts =
  Scripts
    <$> E.poolScript
    <*> E.swapScript
    <*> E.depositScript
    <*> E.redeemScript
