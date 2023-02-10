module Main where

import Prelude (($))
import Prelude qualified as Prel
import Data.Functor qualified as Functor
import Data.Default qualified as Default
import Ledger.Ada qualified as Ada
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Contract.Test qualified as Test

main :: Prel.IO ()
main = Trace.runEmulatorTraceIO' Default.def Default.def trace
    where 
        trace :: Trace.EmulatorTrace ()
        trace = Functor.void 
              $ Trace.payToWallet Test.w1 Test.w2 (Ada.lovelaceValueOf 2_000_000)