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
  = GetLedgerV1
  | UpdateLedgerV1
    { name :: String
    , createdAt :: Instant
    }

derive instance ledgerRequestContractGeneric :: Generic LedgerRequestContract _
instance ledgerRequestContractDecodeJson :: DecodeJson LedgerRequestContract where
  decodeJson a = genericDecodeJson a
instance ledgerRequestContractEncodeJson :: EncodeJson LedgerRequestContract where
  encodeJson a = genericEncodeJson a

