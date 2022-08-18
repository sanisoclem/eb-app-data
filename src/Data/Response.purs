module Data.Response where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson, stringify)
import Data.Argonaut.Core as A
import Data.Tuple (Tuple(..))
import Effect.Aff (Error)
import FFI.DurableObject (DurableObjectResponse, doStringResponse)
import Foreign.Object (fromFoldable)

toDurableObjectResponse :: Response -> DurableObjectResponse
toDurableObjectResponse x = doStringResponse (stringify x.body) x.statusCode

type Response =
  { statusCode :: Int
  , body :: Json
  }

jsonResponse :: ∀ a. (EncodeJson a) => Int -> a -> Response
jsonResponse code = { statusCode: _, body: _ } code <<< encodeJson

messageResponse :: Int -> String -> Response
messageResponse code msg = { statusCode: _, body: _ } code $ json
  where
    json = A.fromObject ( fromFoldable [ Tuple "message" (A.fromString msg) ] )

errorResponse :: Error -> Response
errorResponse err = messageResponse 500 $ show err

notFoundResponse :: String -> Response
notFoundResponse msg = messageResponse 404 $ msg