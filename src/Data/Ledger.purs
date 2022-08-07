module Data.Ledger where

import Prelude

import Data.Argonaut (JsonDecodeError(..))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeNumber)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.DateTime.Instant as InstantImpl
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Time.Duration (Milliseconds(..))
import Safe.Coerce (coerce)

newtype Instant = Instant (InstantImpl.Instant)

data AccountGroup
  = Cash
  | Expense
  | Liability
  | Income

data AccountType
  = Credit
  | Debit

data LedgerEvent
  = UpdateLedger
    { name :: String
    , createdAt :: Instant
    }
  | CreateAccount
    { name :: String
    , group :: AccountGroup
    }

derive instance genericLedgerEvent :: Generic LedgerEvent _
derive instance genericAccountGroup :: Generic AccountGroup _
derive instance genericAccountType :: Generic AccountType _

instance instantDecodeJson :: DecodeJson Instant where
  decodeJson a = do
    milis <- decodeNumber a
    i <- note (UnexpectedValue a) $ InstantImpl.instant (Milliseconds milis)
    pure $ coerce i

instance accountGroupDecodeJson :: DecodeJson AccountGroup where
  decodeJson a = genericDecodeJson a

instance accountTypeDecodeJson :: DecodeJson AccountType where
  decodeJson a = genericDecodeJson a

instance ledgerEventDecodeJson :: DecodeJson LedgerEvent where
  decodeJson a = genericDecodeJson a


-- data Ledger
--   = LedgerV1
--     { name :: String
--     }
--   | LedgerV2
--     { name :: String
--     , createdAt :: Instant
--     }
-- type LedgerDomain =
--   { name :: String
--   , createdAt :: Instant
--   }

-- upgrade :: Ledger -> LedgerDomain
