module EB.DB.Data.Fetch where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson, stringify)
import Data.Argonaut.Core as A
import Data.Tuple (Tuple(..))
import EB.DB.FFI.DurableObject (DurableObjectResponse, doStringResponse)
import Effect.Aff (Error)
import Foreign.Object (fromFoldable)


data RequestMethod
  = GET
  | POST
  | DELETE
  | PUT
  | Unknown String

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