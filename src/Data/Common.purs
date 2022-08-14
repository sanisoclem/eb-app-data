module Data.Common where

import Prelude

import Capability.DataContract (class DocumentId, class RandomId)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeInt)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Encoders (encodeInt)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Document.Id (accountIdPrefix, transactionIdPrefix)
import Data.Generic.Rep (class Generic)
import Data.String (Pattern(..), stripPrefix)
import Safe.Coerce (coerce)

newtype Money = Money Int
derive newtype instance semiringMoney :: Semiring Money
derive newtype instance ringMoney :: Ring Money
derive newtype instance eqMoney :: Eq Money
derive newtype instance commutativeRingMoney :: CommutativeRing Money
derive newtype instance encodeJsonMoney :: EncodeJson Money
derive newtype instance decodeJsonMoney :: DecodeJson Money

mkMoney :: Int -> Money
mkMoney = Money

zeroMoney ∷ Money
zeroMoney = Money 0

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
  fromDocumentId = coerce <<< stripPrefix (Pattern  accountIdPrefix)
  toDocumentId = (<>) accountIdPrefix <<< coerce

instance randomIdAccountId :: RandomId AccountId where
  generate = AccountId

newtype TransactionId = TransactionId String
derive newtype instance decodeJsonTransactionId :: DecodeJson TransactionId
derive newtype instance encodeJsonTransactionId :: EncodeJson TransactionId

instance documentIdTransactionId :: DocumentId TransactionId where
  fromDocumentId = coerce <<< stripPrefix (Pattern transactionIdPrefix)
  toDocumentId = (<>) transactionIdPrefix <<< coerce

instance randomIdTransactionId :: RandomId TransactionId where
  generate = TransactionId

newtype SubscriptionId = SubscriptionId String

derive newtype instance decodeJsonSubscriptionId :: DecodeJson SubscriptionId
derive newtype instance encodeJsonSubscriptionId :: EncodeJson SubscriptionId