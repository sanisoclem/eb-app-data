module FFI.DurableObject
  ( DurableObjectRequest
  , DurableObjectResponse
  , DurableObjectState
  , doRequestGetMethod
  , doStringResponse
  , doRequestGetBody
  , doErrorResponse
  , doNotFoundResponse
  , doGetState
  )
  where

import Data.Argonaut (Json)
import Data.Maybe (Maybe (..))
import Data.Request (RequestMethod(..))
import Effect.Aff (Aff)

foreign import data DurableObjectRequest :: Type
foreign import data DurableObjectState :: Type
foreign import data DurableObjectResponse :: Type

foreign import doStringResponse :: String -> DurableObjectResponse
foreign import doErrorResponse :: String -> DurableObjectResponse
foreign import doNotFoundResponse :: String -> DurableObjectResponse

doRequestGetMethod :: DurableObjectRequest -> RequestMethod
doRequestGetMethod req = case doRequestGetMethodImpl req of
  "POST" -> POST
  "GET" -> GET
  "DELETE" -> DELETE
  "PUT" -> PUT
  x -> Unknown x
doGetState :: DurableObjectState -> String -> Aff (Maybe Json)
doGetState = doGetStateImpl Just Nothing

foreign import doRequestGetBody :: DurableObjectRequest -> Aff String

-- private
foreign import doRequestGetMethodImpl :: DurableObjectRequest -> String
foreign import doGetStateImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> DurableObjectState -> String -> Aff (Maybe Json)

