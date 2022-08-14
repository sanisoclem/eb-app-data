module Data.Contract.Document.DurableObject where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, (.:), (:=), (~>))
import Data.Serialization (bodyTag, versionTag)

data OutboxContract =
  V1 { toSend :: Array String }

instance encodeJsonOutboxContract :: EncodeJson OutboxContract where
  encodeJson (V1 x) = versionTag := 1 ~> bodyTag := x
instance decodeJsonOutboxContract :: DecodeJson OutboxContract where
  decodeJson json = do
    obj <- decodeJson json
    obj .: versionTag >>= case _ of
      1 -> V1 <$> obj .: bodyTag
      _ -> throwError $ UnexpectedValue json