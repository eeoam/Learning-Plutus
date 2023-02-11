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
import Plutus.Contract (Contract, Empty)
import Plutus.Contract qualified as Contract
import {- plutus-ledger-constraints -} Ledger.Constraints qualified as Constraints
import {- plutus-ledger -} Ledger qualified as Ledger

main :: Prel.IO ()
main = Trace.runEmulatorTraceIO' Default.def Default.def trace
    where 
        trace :: Trace.EmulatorTrace ()
        trace = Functor.void $ do
                    Extras.logInfo @Prel.String "Running contract..."
                    let ppkh = Test.mockWalletPaymentPubKeyHash Test.w2
                    Trace.activateContractWallet Test.w1 $ send_ada ppkh

        send_ada :: Ledger.PaymentPubKeyHash -> Contract () Empty Text ()
        send_ada ppkh = do
            let ada = Ada.lovelaceValueOf 5_000_000 
            let constraint = Constraints.mustPayToPubKey ppkh ada
            tx <- Contract.submitTx constraint
            Functor.void $ Contract.awaitTxConfirmed (Ledger.getCardanoTxId tx)
            Contract.logInfo @Prel.String "Sent ada!"