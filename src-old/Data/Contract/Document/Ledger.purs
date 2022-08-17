module Data.Contract.Document.Ledger where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Common (Instant)
import Data.Generic.Rep (class Generic)

newtype LedgerDocumentContract = LedgerDocumentContractV1
  { name :: String
  , createdAt :: Instant
  }

derive instance ledgerDocumentGeneric :: Generic LedgerDocumentContract _
instance ledgerDocumentEncodeJson :: EncodeJson LedgerDocumentContract where
  encodeJson a = genericEncodeJson a

instance ledgerDocumentDecodeJson :: DecodeJson LedgerDocumentContract where
  decodeJson a = genericDecodeJson a
