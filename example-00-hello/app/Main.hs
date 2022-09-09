module Main where

import Control.Monad.Freer.Extras (logInfo)
import Data.Default(def)
import Plutus.Trace.Emulator (
    EmulatorTrace,
    runEmulatorTraceIO')



trace :: EmulatorTrace ()
trace = logInfo @String "Hello!"

main :: IO ()
main = runEmulatorTraceIO' def def trace