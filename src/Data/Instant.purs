module EB.DB.Data.Instant where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..))
import Data.Argonaut.Decode.Decoders (decodeNumber)
import Data.Argonaut.Encode.Encoders (encodeNumber)
import Data.DateTime.Instant as StdInstant
import Data.Either (note)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as E

newtype Instant = Instant StdInstant.Instant

derive newtype instance Show Instant
derive newtype instance Eq Instant
derive newtype instance Ord Instant
derive newtype instance Bounded Instant

mkInstant :: Number -> Maybe Instant
mkInstant x = map Instant <<< StdInstant.instant <<< Milliseconds $ x
unInstant :: Instant -> Number
unInstant (Instant x) = unwrap <<< StdInstant.unInstant $ x

now :: forall m. MonadEffect m => m Instant
now = Instant <$> liftEffect E.now

instance instantDecodeJson :: DecodeJson Instant where
  decodeJson a = do
    x <- decodeNumber a
    note (UnexpectedValue a) $ mkInstant x
instance instantEncodeJson :: EncodeJson Instant where
  encodeJson a = encodeNumber $ unInstant a

