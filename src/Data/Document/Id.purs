module Data.Document.Id where

import Prelude

import Capability.RandomId (class RandomId)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Safe.Coerce (coerce)

accountIdPrefix :: String
accountIdPrefix = "act"

transactionIdPrefix :: String
transactionIdPrefix = "txn/"

indexIdPrefix :: String
indexIdPrefix = "idx/"

newtype AccountId = AccountId String
derive newtype instance decodeJsonAccountId :: DecodeJson AccountId
derive newtype instance encodeJsonAccountId :: EncodeJson AccountId
instance randomIdAccountId :: RandomId AccountId where
  generate = AccountId

newtype TransactionId = TransactionId String
derive newtype instance decodeJsonTransactionId :: DecodeJson TransactionId
derive newtype instance encodeJsonTransactionId :: EncodeJson TransactionId
instance randomIdTransactionId :: RandomId TransactionId where
  generate = TransactionId

newtype IndexId = IndexId String
derive newtype instance decodeJsonIndexId :: DecodeJson IndexId
derive newtype instance encodeJsonIndexId :: EncodeJson IndexId

indexId :: String -> IndexId
indexId = IndexId

data DocumentId
  = LedgerId
  | AccountCollection AccountId
  | TransactionCollection TransactionId
  | IndexCollection IndexedCollection

data IndexedCollection
  = TransactionIndex IndexId
  | AccountIndex IndexId

documentIdString :: DocumentId -> String
documentIdString LedgerId = "ledger"
documentIdString (AccountCollection id) = accountIdPrefix <> "/" <> coerce id
documentIdString (TransactionCollection id) = transactionIdPrefix <> "/" <> coerce id
documentIdString (IndexCollection idC) = indexIdPrefix <> "/" <> case idC of
  TransactionIndex idF -> transactionIdPrefix <> "/" <> (coerce idF)
  AccountIndex idF -> accountIdPrefix <> "/" <> (coerce idF)