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

type Schema = Contract.Endpoint "give" Ledger.PaymentPubKeyHash

main :: Prel.IO ()
main = Trace.runEmulatorTraceIO' Default.def Default.def trace
    where 
        trace :: Trace.EmulatorTrace ()
        trace = Functor.void $ do
                    Extras.logInfo @Prel.String "Activating wallet..."
                    handle <- Trace.activateContractWallet Test.w1 go
                    let ppkh = Test.mockWalletPaymentPubKeyHash Test.w2
                    Trace.callEndpoint @"give" handle ppkh

        go :: Contract () Schema Text ()
        go = (Contract.awaitPromise $ Contract.endpoint @"give" send_ada) Prel.>> go

        send_ada :: Ledger.PaymentPubKeyHash -> Contract () Schema Text ()
        send_ada ppkh = do
            let ada = Ada.lovelaceValueOf 1_000_000 
            let constraint = Constraints.mustPayToPubKey ppkh ada
            tx <- Contract.submitTx constraint
            Functor.void $ Contract.awaitTxConfirmed (Ledger.getCardanoTxId tx)
       