module Main where

import Prelude qualified as Prel
import Control.Monad.Freer.Extras qualified as Extras
import Data.Default qualified as Default
import Plutus.Trace.Emulator qualified as Trace

main :: Prel.IO ()
main = Trace.runEmulatorTraceIO' Default.def Default.def trace
    where
        trace :: Trace.EmulatorTrace ()
        trace = Extras.logInfo @Prel.String "Hello!"

