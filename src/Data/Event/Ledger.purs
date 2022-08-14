module Data.Event.Ledger where

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))


data LedgerEvent = TODO

instance encodeDataContractLedgerEvent :: EncodeDataContract LedgerEvent LedgerEvent where
  toContract x = x

instance decodeDataContractLedgerEvent :: DecodeDataContract LedgerEvent LedgerEvent where
  fromContract = Just

derive instance genericLedgerEvent :: Generic LedgerEvent _
instance encodeJsonLedgerEvent :: EncodeJson LedgerEvent where
  encodeJson a = genericEncodeJson a

instance decodeJsonLedgerEvent :: DecodeJson LedgerEvent where
  decodeJson a = genericDecodeJson a
