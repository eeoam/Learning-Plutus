{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Map qualified as Map
import Data.Functor (void)
import Control.Monad.Freer.Extras (logInfo)
import Data.Default(def)
import Ledger.Value (Value, AssetClass (..), assetClassValue)
import Ledger.Ada (lovelaceValueOf)
import Plutus.Trace.Emulator (
    EmulatorTrace,
    payToWallet,
    runEmulatorTraceIO')
import Plutus.Contract.Test (w1, w2)
import SendAda.OffChain (sendAda)

trace :: EmulatorTrace ()
trace = void $ payToWallet w1 w2 $ lovelaceValueOf 1_000_000

main :: IO ()
main = runEmulatorTraceIO' def def trace