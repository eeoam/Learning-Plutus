module Main where

import Prelude (($))
import Prelude qualified as Prel
import Data.Text (Text)
import Data.Functor qualified as Functor
import Data.Default qualified as Default
import Control.Monad.Freer.Extras qualified as Extras
import Ledger.Ada qualified as Ada
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Contract.Test qualified as Test
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract

main :: Prel.IO ()
main = Trace.runEmulatorTraceIO' Default.def Default.def trace
    where 
        trace :: Trace.EmulatorTrace ()
        trace = Functor.void $ do
                    Extras.logInfo @Prel.String "Running contract..."
                    Trace.activateContractWallet Test.w1 hello

        hello :: Contract () Empty Text ()
        hello = do
            Contract.logInfo @Prel.String "Hello from contract!"