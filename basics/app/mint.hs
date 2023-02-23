module Main where

import Prelude (($), (<*>), (>>), IO, return, Bool(..))
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
import Plutus.Contract.Test.ContractModel qualified as CM
import {- QuickCheck -} Test.QuickCheck qualified as QC

import {- plutus-ledger-api -} Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import {- plutus-script-utils -} Plutus.Script.Utils.Typed qualified as Utils
import {- plutus-script-utils -} Plutus.Script.Utils.V2.Scripts qualified as S2

import Ledger.Typed.Scripts qualified as LS

import Ledger.Value qualified as Value

data MODEL = MODEL
    { _wallet :: Test.Wallet
    } deriving stock (Prel.Show, Prel.Eq, Generic)
Lens.makeLenses ''MODEL

type Schema = Contract.Endpoint "mint" ()

mintEp 
    :: forall w s e. 
    ( Contract.HasEndpoint "mint" () s
    , Contract.AsContractError e
    )
    => Contract.Promise w s e ()
mintEp = Contract.endpoint @"mint" $ Prel.const mint

mint :: Contract.AsContractError e => Contract w s e ()
mint = do
    let constraint = Constraints.mustMintCurrency (S2.mintingPolicyHash policy) "ABC" 1 
    -- tx <- Contract.submitTx constraint
    tx <- Contract.submitTxConstraintsWith @LS.Any (Constraints.plutusV2MintingPolicy policy) constraint
    Functor.void $ Contract.awaitTxConfirmed (Ledger.getCardanoTxId tx)

{-# INLINABLE plutus #-}
plutus :: () -> V2.ScriptContext  -> Bool
plutus () _ = True

policy :: V2.MintingPolicy
policy = V2.mkMintingPolicyScript 
    $$(PlutusTx.compile [|| Utils.mkUntypedMintingPolicy plutus ||])

currency :: Value.CurrencySymbol
currency = S2.scriptCurrencySymbol policy

token :: V2.TokenName
token = "ABC"

deriving instance Prel.Show (CM.ContractInstanceKey MODEL w s e p)
deriving instance Prel.Eq (CM.ContractInstanceKey MODEL w s e p)

instance CM.ContractModel MODEL where
    data ContractInstanceKey MODEL w s e p where
        CIK :: Test.Wallet -> CM.ContractInstanceKey MODEL () Schema Text ()

    instanceWallet :: CM.ContractInstanceKey MODEL w s e p -> Test.Wallet
    instanceWallet (CIK w) = w

    initialInstances :: [ CM.StartContract MODEL ]
    initialInstances = [ CM.StartContract (CIK Test.w1) () ]

    instanceContract
        :: (CM.SymToken -> Ledger.AssetClass)
        -> CM.ContractInstanceKey MODEL w s e p
        -> p -> Contract w s e ()
    instanceContract _ (CIK _) _ = test
        where 
            test = Contract.awaitPromise mintEp >> test

    data Action MODEL
        = Mint Test.Wallet
        deriving stock (Prel.Show, Prel.Eq, Generic)

    arbitraryAction :: CM.ModelState MODEL -> QC.Gen (CM.Action MODEL)
    arbitraryAction _ 
        = QC.oneof [ Mint <$> QC.elements [ Test.w1 ] ]

    perform handle _ _ (Mint qianbao) = do
        Trace.callEndpoint @"mint" (handle $ CIK qianbao) ()
        CM.delay 1

    initialState :: MODEL
    initialState = MODEL Test.w1

    nextState :: CM.Action MODEL -> CM.Spec MODEL ()
    nextState (Mint qianbao) = do
        let val = Value.singleton currency token 1
        CM.mint val
        CM.deposit qianbao val
        -- v <- CM.askModelState CM.lockedValue
        CM.wait 2

property :: CM.Actions MODEL -> QC.Property
property = CM.propRunActions_

main :: IO ()
main = do
    Prel.putStrLn "A simple minting example."
    QC.quickCheck property

