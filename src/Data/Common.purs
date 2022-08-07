module Data.Common where

import Prelude

import Capability.DataContract (class DataContract)
import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeInt)
import Data.Argonaut.Encode.Encoders (encodeInt)

newtype Instant = Instant Int

mkInstant :: Int -> Instant
mkInstant = Instant

unInstant :: Instant -> Int
unInstant (Instant x) = x

instance instantDataContract :: DataContract Int Instant where
  fromContract = mkInstant
  toContract = unInstant

instance instantDecodeJson :: DecodeJson Instant where
  decodeJson a = mkInstant <$> decodeInt a
instance instantEncodeJson :: EncodeJson Instant where
  encodeJson a = encodeInt $ unInstant a