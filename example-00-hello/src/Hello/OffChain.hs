-- | The obligatory "Hello" program for Plutus.
{-# LANGUAGE NoImplicitPrelude #-}

module Hello.OffChain where

import Data.Text (Text)
import Plutus.Contract
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Prelude (String)

type Schema =
        Endpoint "hello" ()

hello :: Contract () Schema Text ()
hello = logInfo @String "Hello from off-chain code!"
