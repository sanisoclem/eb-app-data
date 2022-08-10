module Data.Common where

import Prelude

import Capability.DataContract (class DocumentId, class RandomId)
import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeInt)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Encoders (encodeInt)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.String (Pattern(..), stripPrefix)
import Safe.Coerce (coerce)

-- types that we don't need to version
-- rules:
--   - Don't remove constructors (enums)
--   - Don't implement DataContract (no versioning)
--   - Implement EncodeJson and DecodeJson

newtype Instant = Instant Int

mkInstant :: Int -> Instant
mkInstant = Instant

unInstant :: Instant -> Int
unInstant (Instant x) = x

instance instantDecodeJson :: DecodeJson Instant where
  decodeJson a = mkInstant <$> decodeInt a
instance instantEncodeJson :: EncodeJson Instant where
  encodeJson a = encodeInt $ unInstant a

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
derive newtype instance decodeJsonAccountId :: DecodeJson AccountId
derive newtype instance encodeJsonAccountId :: EncodeJson AccountId
instance documentIdAccountId :: DocumentId AccountId where
  fromDocumentId = coerce <<< stripPrefix (Pattern "account/")
  toDocumentId = (<>) "account/" <<< coerce
instance randomIdAccountId :: RandomId AccountId where
  generate = AccountId

newtype TransactionId = TransactionId String
derive newtype instance decodeJsonTransactionId :: DecodeJson TransactionId
derive newtype instance encodeJsonTransactionId :: EncodeJson TransactionId
newtype SubscriptionId = SubscriptionId String
derive newtype instance decodeJsonSubscriptionId :: DecodeJson SubscriptionId
derive newtype instance encodeJsonSubscriptionId :: EncodeJson SubscriptionId

newtype VersionNumber = VersionNumber Int
derive newtype instance decodeJsonVersionNumber :: DecodeJson VersionNumber
derive newtype instance encodeJsonVersionNumber :: EncodeJson VersionNumber