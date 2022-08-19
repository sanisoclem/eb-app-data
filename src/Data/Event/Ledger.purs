module Data.Event.Ledger where

import Prelude
import Data.Generic.Rep (class Generic)
import Capability.Storage.Outbox (class OutboxEvent)
import Capability.Utility (convertJsonErrorToError)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)

data LedgerEvent
  = LedgerUpdated
  | AccountCreated
  | AccountUpdated
  | AccountClosed
  | BalanceUpdated
  | TransactionCreated
  | TransactionUpdated
  | TransactionDeleted

derive instance Generic LedgerEvent _
instance OutboxEvent LedgerEvent where
  encodeEvent = genericEncodeJson
  decodeEvent = convertJsonErrorToError <<< genericDecodeJson