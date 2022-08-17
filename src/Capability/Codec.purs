module Capability.Codec where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Exception (Error, error)


class Encodable a where
  encode :: a -> Json

class Decodable a where
  decode :: Json -> Either Error a

instance encodableJsonEncode :: (EncodeJson a) => Encodable a where
  encode = encodeJson
instance decodableJsonDecode :: (DecodeJson a) => Decodable a where
  decode = lmap (error <<< printJsonDecodeError) <<< decodeJson