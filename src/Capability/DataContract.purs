module Capability.DataContract where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Maybe (Maybe)
import Effect.Exception (Error, error)

class (DecodeJson vd) <= DecodeDataContract vd d | d -> vd where
  fromContract :: vd -> Maybe d

class (EncodeJson vd) <= EncodeDataContract vd d | d -> vd where
  toContract :: d -> vd

decodeContractJson :: ∀ a b. (DecodeDataContract a b) => Json -> Either Error b
decodeContractJson = (note (error "Failed to convert from contract") <<< fromContract) <=< lmap error <<< lmap printJsonDecodeError <<< decodeJson

encodeContractJson :: ∀ a b. (EncodeDataContract a b) => b -> Json
encodeContractJson = encodeJson <<< toContract