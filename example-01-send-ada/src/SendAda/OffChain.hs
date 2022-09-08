{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SendAda.OffChain where

import Data.Text (Text)
import Plutus.Contract
import PlutusTx.Prelude (Integer)
import Ledger (PaymentPubKeyHash, getCardanoTxId)
import Ledger.Ada as Ada
import Ledger.Constraints (mustPayToPubKey)
import Prelude (String, Show (..), ($))
import Text.Printf (printf)
import Control.Monad (void)

type Schema =
        Endpoint "payToWallet" (PaymentPubKeyHash, Integer)

sendAda :: PaymentPubKeyHash -> Integer -> Contract () Schema Text ()
sendAda ppkh amt = do
    let tx = mustPayToPubKey ppkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s" amt (show ppkh)

