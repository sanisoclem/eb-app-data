module Data.Response where

import Prelude

import Capability.DataContract (class EncodeDataContract, encodeContractJson)
import Data.Argonaut (class EncodeJson, stringify)
import Effect.Aff (Error)
import FFI.DurableObject (DurableObjectResponse, doStringResponse)

stringResponse :: ∀ m. (Applicative m) => String -> m DurableObjectResponse
stringResponse resp = pure $ doStringResponse resp 200

jsonResponse :: ∀ a b m. (Applicative m) => (EncodeJson a) => (EncodeDataContract a b) => b -> m DurableObjectResponse
jsonResponse = encodeContractJson >>> stringify >>> stringResponse

notFoundResponse :: ∀ m. (Applicative m) => String -> m DurableObjectResponse
notFoundResponse msg = pure $ doStringResponse msg 404

errorResponse :: ∀ m. (Applicative m) => Error -> m DurableObjectResponse
errorResponse err = pure $ doStringResponse (show err) 500