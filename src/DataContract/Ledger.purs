module DataContract.Ledger where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Common (AccountId, AccountType, Denomination, Instant, SubscriptionId, TransactionId)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)

data LedgerDocumentContract
  = LedgerDocumentContractV1
    { name :: String
    , createdAt :: Instant
    }
derive instance ledgerDocumentGeneric :: Generic LedgerDocumentContract _
instance ledgerDocumentEncodeJson :: EncodeJson LedgerDocumentContract where
  encodeJson a = genericEncodeJson a
instance ledgerDocumentDecodeJson :: DecodeJson LedgerDocumentContract where
  decodeJson a = genericDecodeJson a

data LedgerRequestContract
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
    { credit :: Maybe AccountId
    , debit :: Maybe AccountId
    , amount :: String
    , notes :: String
    }
  | UpdateTransactionV1
    { transactionId :: TransactionId
    , credit :: Maybe AccountId
    , debit :: Maybe AccountId
    , amount :: String
    , notes :: String
    }
  | DeleteTransactionV1 TransactionId
  | GetLedgerV1
  | GetTransactionsV1
  | SubscribeV1 String
  | UnsubscribeV1 SubscriptionId

derive instance ledgerCommandContractGeneric :: Generic LedgerRequestContract _
instance ledgerRequestContractDecodeJson :: DecodeJson LedgerRequestContract where
  decodeJson a = genericDecodeJson a
instance ledgerRequestContractEncodeJson :: EncodeJson LedgerRequestContract where
  encodeJson a = genericEncodeJson a
