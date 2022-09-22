module EB.DB.Data.Event.Ledger where

import Prelude

import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import EB.DB.Capability.Outbox (class OutboxEvent)
import EB.DB.Data.Utility (convertJsonErrorToError)

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