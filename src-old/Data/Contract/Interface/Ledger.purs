module Data.Contract.Interface.Ledger where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Common (AccountId, AccountType, Denomination, Money, TransactionId)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)

data LedgerCommandContract
  = UpdateLedgerV1
      { name :: String
      }
  | CreateAccountV1
      { name :: String
      , accountType :: AccountType
      , denomination :: Denomination
      }
  | UpdateAccountV1
      { accountId :: AccountId
      , name :: String
      }
  | CloseAccountV1 AccountId
  | CreateTransactionV1
      { sortKey :: Int
      , credit :: Maybe AccountId
      , debit :: Maybe AccountId
      , amount :: Money
      , notes :: String
      }
  | UpdateTransactionV1
      { sortKey :: Int
      , transactionId :: TransactionId
      , credit :: Maybe AccountId
      , debit :: Maybe AccountId
      , amount :: Money
      , notes :: String
      }
  | DeleteTransactionV1 TransactionId

derive instance genericLedgerCommandContract :: Generic LedgerCommandContract _
instance decodeJsonLedgerCommandContract :: DecodeJson LedgerCommandContract where
  decodeJson a = genericDecodeJson a

instance encodeJsonLedgerCommandContract :: EncodeJson LedgerCommandContract where
  encodeJson a = genericEncodeJson a

data LedgerQueryContract
  = GetLedgerV1
  | GetTransactionsV1

derive instance genericLedgerQueryContract :: Generic LedgerQueryContract _
instance decodeJsonLedgerQueryContract :: DecodeJson LedgerQueryContract where
  decodeJson a = genericDecodeJson a

instance encodeJsonLedgerQueryContract :: EncodeJson LedgerQueryContract where
  encodeJson a = genericEncodeJson a
