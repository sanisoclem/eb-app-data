module Data.Contract.Document.DurableObject where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, (.:), (:=), (~>))
import Data.Argonaut.Decode.Class (class DecodeJsonField)
import Data.Common (IndexId(..), IndexPageId)
import Data.Map (Map)
import Data.Serialization (bodyTag, versionTag)

data OutboxContract =
  OutboxContractV1 { toSend :: Array String }

instance encodeJsonOutboxContract :: EncodeJson OutboxContract where
  encodeJson (OutboxContractV1 x) = versionTag := 1 ~> bodyTag := x
instance decodeJsonOutboxContract :: DecodeJson OutboxContract where
  decodeJson json = do
    obj <- decodeJson json
    obj .: versionTag >>= case _ of
      1 -> OutboxContractV1 <$> obj .: bodyTag
      _ -> throwError $ UnexpectedValue json


data IndexContract a
  = IndexContractV1
    { indexId :: IndexId
    , pageSize :: Int
    , pages :: Map a IndexPageId
    }

instance encodeJsonIndexContract :: (EncodeJson a, Ord a) => EncodeJson (IndexContract a) where
  encodeJson (IndexContractV1 x) = versionTag := 1 ~> bodyTag := x
instance decodeJsonIndexContract :: (DecodeJson a, Ord a) => DecodeJson (IndexContract a) where
  decodeJson json = do
    obj <- decodeJson json
    obj .: versionTag >>= case _ of
      1 -> IndexContractV1 <$> obj .: bodyTag
      _ -> throwError $ UnexpectedValue json

data IndexPageContract a
  = IndexPageContractV1
    { pageId :: IndexPageId
    , docIds :: Array { key :: a, docId :: String }
    }

instance encodeJsonIndexPageContract :: (EncodeJson a, Ord a) => EncodeJson (IndexPageContract a) where
  encodeJson (IndexPageContractV1 x) = versionTag := 1 ~> bodyTag := x
instance decodeJsonIndexPageContract :: (DecodeJsonField a, Ord a) => DecodeJson (IndexPageContract a) where
  decodeJson json = do
    obj <- decodeJson json
    obj .: versionTag >>= case _ of
      1 -> IndexPageContractV1 <$> obj .: bodyTag
      _ -> throwError $ UnexpectedValue json