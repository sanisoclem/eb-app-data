module DataContract.Ledger where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Common (Instant)
import Data.Generic.Rep (class Generic)

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
  = LedgerQueryContract LedgerQueryContract
  | LedgerCommandContract LedgerCommandContract
  | LedgerSubscriptionContract LedgerSubscriptionContract
derive instance ledgerRequestContractGeneric :: Generic LedgerRequestContract _
instance ledgerRequestContractDecodeJson :: DecodeJson LedgerRequestContract where
  decodeJson a = genericDecodeJson a
instance ledgerRequestContractEncodeJson :: EncodeJson LedgerRequestContract where
  encodeJson a = genericEncodeJson a


data LedgerCommandContract
  = UpdateLedgerV1
  | CreateAccountV1
  | UpdateAccountV1
  | CloseAccountV1
  | CreateTransactionV1
  | UpdateTransactionV1
  | DeleteTransactionV1
derive instance ledgerCommandContractGeneric :: Generic LedgerCommandContract _
instance ledgerCommandContractDecodeJson :: DecodeJson LedgerCommandContract where
  decodeJson a = genericDecodeJson a
instance ledgerCommandContractEncodeJson :: EncodeJson LedgerCommandContract where
  encodeJson a = genericEncodeJson a

data LedgerQueryContract
  = GetLedgerV1
  | GetAccountsV1
  | GetTransactionsV1
derive instance ledgerQueryContractGeneric :: Generic LedgerQueryContract _
instance ledgerQueryContractDecodeJson :: DecodeJson LedgerQueryContract where
  decodeJson a = genericDecodeJson a
instance ledgerQueryContractEncodeJson :: EncodeJson LedgerQueryContract where
  encodeJson a = genericEncodeJson a

data LedgerSubscriptionContract
  = SubscribeV1
  | UnsubscribeV1

derive instance ledgerSubscriptionContractGeneric :: Generic LedgerSubscriptionContract _
instance ledgerSubscriptionContractDecodeJson :: DecodeJson LedgerSubscriptionContract where
  decodeJson a = genericDecodeJson a
instance ledgerSubscriptionContractEncodeJson :: EncodeJson LedgerSubscriptionContract where
  encodeJson a = genericEncodeJson a
