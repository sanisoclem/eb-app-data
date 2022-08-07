module Data.Response where

import Prelude

import Capability.DataContract (class DataContract, toContract)
import Data.Argonaut (class EncodeJson, encodeJson, stringify)
import Effect.Aff (Error)
import FFI.DurableObject (DurableObjectResponse, doStringResponse)

stringResponse :: forall m. (Applicative m) => String -> m DurableObjectResponse
stringResponse resp = pure $ doStringResponse resp 200

jsonResponse :: forall a b m. (Applicative m) => (EncodeJson a) => (DataContract a b) => b -> m DurableObjectResponse
jsonResponse = toContract >>> encodeJson >>> stringify >>> stringResponse

notFoundResponse :: forall m. (Applicative m) => String -> m DurableObjectResponse
notFoundResponse msg = pure $ doStringResponse msg 404

errorResponse :: forall m. (Applicative m) => Error -> m DurableObjectResponse
errorResponse err = pure $ doStringResponse (show err) 500