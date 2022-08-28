module Data.Instant where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeInt)
import Data.Argonaut.Encode.Encoders (encodeInt)

newtype Instant = Instant Int

derive newtype instance Show Instant
derive newtype instance Eq Instant
derive newtype instance Ord Instant

mkInstant :: Int -> Instant
mkInstant = Instant
unInstant :: Instant -> Int
unInstant (Instant x) = x
instance instantDecodeJson :: DecodeJson Instant where
  decodeJson a = mkInstant <$> decodeInt a
instance instantEncodeJson :: EncodeJson Instant where
  encodeJson a = encodeInt $ unInstant a