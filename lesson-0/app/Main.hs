module Main where

import Data.Functor (void)
import Data.Default(def)
import Plutus.Trace.Emulator (
    EmulatorTrace,
    activateContractWallet, waitNSlots, runEmulatorTraceIO')
import Plutus.Contract.Trace (knownWallet)
import Hello.OffChain (hello)

trace :: EmulatorTrace ()
trace = do
    void $ activateContractWallet (knownWallet 1) hello
    void $ waitNSlots 2

main :: IO ()
main = runEmulatorTraceIO' def def trace