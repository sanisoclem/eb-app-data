module Data.Common where

import Prelude

import Capability.RandomId (class RandomId)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeInt)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)

data AccountType
  = Income
  | Expense
  | Liability
  | Asset

derive instance genericAccountType :: Generic AccountType _
instance decodeJsonAccountType :: DecodeJson AccountType where
  decodeJson = genericDecodeJson
instance encodeJsonAccountType :: EncodeJson AccountType where
  encodeJson = genericEncodeJson

data Denomination
  = Currency String
  | Equity String

derive instance genericDenomination :: Generic Denomination _
instance decodeJsonDenomination :: DecodeJson Denomination where
  decodeJson = genericDecodeJson
instance encodeJsonDenomination :: EncodeJson Denomination where
  encodeJson = genericEncodeJson

