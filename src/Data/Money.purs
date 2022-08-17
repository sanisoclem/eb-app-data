module Data.Money where

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson)

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