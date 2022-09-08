module EB.DB.Data.Money where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Decimal (Decimal)
import Data.Decimal as D
import Data.Either (note)
import Data.Maybe (Maybe)
import Safe.Coerce (coerce)

newtype Money = Money Decimal
derive newtype instance Semiring Money
derive newtype instance Ring Money
derive newtype instance Eq Money
derive newtype instance CommutativeRing Money

fromString :: String -> Maybe Money
fromString = map Money <<< D.fromString

toString ∷ Money -> String
toString = D.toString <<< coerce

zeroMoney :: Money
zeroMoney = Money $ D.fromInt 0

instance EncodeJson Money where
  encodeJson = encodeJson <<< toString

instance DecodeJson Money where
  decodeJson = (note (TypeMismatch "invalid decimal") <<< fromString) <=< decodeJson