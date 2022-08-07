module Capability.DataContract where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)

class (DecodeJson vd) <= DataContract vd d | d -> vd where
  fromContract :: vd -> d
  toContract :: d -> vd

decodeContractJson :: forall a b. (DecodeJson a) => (DataContract a b) => Json -> Either JsonDecodeError b
decodeContractJson = map fromContract <<< decodeJson

encodeContractJson :: forall a b. (EncodeJson a) => (DataContract a b) => b -> Json
encodeContractJson = encodeJson <<< toContract