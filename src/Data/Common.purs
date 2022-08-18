module Data.Common where

import Prelude

import Capability.RandomId (class RandomId)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Safe.Coerce (coerce)

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

newtype AccountId = AccountId String
unAccountId ∷ AccountId → String
unAccountId = coerce
accountId :: String -> AccountId
accountId = coerce
derive newtype instance decodeJsonAccountId :: DecodeJson AccountId
derive newtype instance encodeJsonAccountId :: EncodeJson AccountId
instance randomIdAccountId :: RandomId AccountId where
  generate = AccountId

newtype TransactionId = TransactionId String
unTransactionId ∷ TransactionId → String
unTransactionId = coerce
transactionId :: String -> TransactionId
transactionId = coerce
derive newtype instance decodeJsonTransactionId :: DecodeJson TransactionId
derive newtype instance encodeJsonTransactionId :: EncodeJson TransactionId
instance randomIdTransactionId :: RandomId TransactionId where
  generate = TransactionId

newtype LedgerId = LedgerId Unit
ledgerId :: LedgerId
ledgerId = LedgerId unit