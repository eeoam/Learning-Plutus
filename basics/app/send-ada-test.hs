module Main where

import Prelude (($), (<*>), (>>))
import Prelude qualified as Prel
import {- lens -} Control.Lens qualified as Lens
import Data.Text (Text)
import Data.Functor ((<$>))
import Data.Functor qualified as Functor
import Data.Default qualified as Default
import Control.Monad.Freer.Extras qualified as Extras
import GHC.Generics (Generic)
import Ledger.Ada qualified as Ada
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Contract.Test qualified as Test
import Plutus.Contract (Contract, Empty)
import Plutus.Contract qualified as Contract
import {- plutus-ledger-constraints -} Ledger.Constraints qualified as Constraints
import {- plutus-ledger -} Ledger qualified as Ledger
import Plutus.Contract.Test.ContractModel qualified as Model
import {- QuickCheck -} Test.QuickCheck qualified as QCheck

data MODEL  = MODEL
    { _donor, _donee :: Test.Wallet
    } deriving stock (Prel.Show, Prel.Eq, Generic)
Lens.makeLenses ''MODEL

instance Model.ContractModel MODEL where
    data Action MODEL
        = Give Test.Wallet Test.Wallet
        deriving stock (Prel.Show, Prel.Eq, Generic)

    {-perform :: Model.HandleFun MODEL
            -> (Model.SymToken -> Ledger.AssetClass)
            -> Model.ModelState MODEL
            -> Model.Action MODEL
            -> Model.SpecificationEmulatorTrace ()-}
    perform handle _ _ (Give donor donee) = do
        let ppkh = Test.mockWalletPaymentPubKeyHash Test.w2
        Trace.callEndpoint @"give" (handle $ KEY donor) ppkh
        Model.delay 1

    arbitraryAction :: Model.ModelState MODEL -> QCheck.Gen (Model.Action MODEL) 
    arbitraryAction _ = QCheck.oneof [ Give <$> els [ Test.w1 ] <*> els [ Test.w2 ] ]
        where els = QCheck.elements

    initialState :: MODEL
    initialState = MODEL Test.w1 Test.w2

    nextState :: Model.Action MODEL -> Model.Spec MODEL ()
    nextState (Give donor donee) = do
        Model.withdraw donor (Ada.adaValueOf 2)
        -- Model.deposit donee (Ada.adaValueOf 1)
        Model.wait 1

    precondition :: Model.ModelState MODEL -> Model.Action MODEL -> Prel.Bool
    precondition s (Give donor donee) = Prel.True
        {- Model.symIsZero (s Lens.^. Model.balanceChange donor) -}

    data ContractInstanceKey MODEL w s e p where
        KEY :: Test.Wallet -> Model.ContractInstanceKey MODEL () Schema Text ()

    initialInstances :: [ Model.StartContract MODEL ]
    initialInstances = [ Model.StartContract (KEY Test.w1) ()]

    instanceWallet :: Model.ContractInstanceKey MODEL w s e p -> Test.Wallet
    instanceWallet (KEY w) = w

    instanceContract :: (Model.SymToken -> Ledger.AssetClass) -> Model.ContractInstanceKey MODEL w s e p -> p -> Contract w s e ()
    instanceContract _ (KEY _) _ = go
    
deriving instance Prel.Show (Model.ContractInstanceKey MODEL w s e p)
deriving instance Prel.Eq   (Model.ContractInstanceKey MODEL w s e p)

property :: Model.Actions MODEL -> QCheck.Property
property = Model.propRunActions_




main :: Prel.IO ()
main = Trace.runEmulatorTraceIO' Default.def Default.def trace
    
trace :: Trace.EmulatorTrace ()
trace = Functor.void $ do
            Extras.logInfo @Prel.String "Activating wallet..."
            handle <- Trace.activateContractWallet Test.w1 go
            let ppkh = Test.mockWalletPaymentPubKeyHash Test.w2
            Trace.callEndpoint @"give" handle ppkh

type Schema = Contract.Endpoint "give" Ledger.PaymentPubKeyHash

go :: Contract () Schema Text ()
go = (Contract.awaitPromise $ Contract.endpoint @"give" send_ada)  >> go

--send_ada :: Ledger.PaymentPubKeyHash -> Contract () Schema Text ()
send_ada :: Contract.AsContractError e => Ledger.PaymentPubKeyHash -> Contract w s e ()
send_ada ppkh = do
    let ada = Ada.lovelaceValueOf 1_000_000 
    let constraint = Constraints.mustPayToPubKey ppkh ada
    tx <- Contract.submitTx constraint
    Functor.void $ Contract.awaitTxConfirmed (Ledger.getCardanoTxId tx)