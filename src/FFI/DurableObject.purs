module FFI.DurableObject
  ( DurableObjectRequest
  , DurableObjectResponse
  , DurableObjectState
  , doRequestGetMethod
  , doStringResponse
  , doRequestGetBody
  , doErrorResponse
  )
  where

import Data.Request (RequestMethod(..))
import Effect.Aff (Aff)

foreign import data DurableObjectRequest :: Type
foreign import data DurableObjectState :: Type
foreign import data DurableObjectResponse :: Type

foreign import doStringResponse :: String -> DurableObjectResponse
foreign import doErrorResponse :: String -> DurableObjectResponse

doRequestGetMethod :: DurableObjectRequest -> RequestMethod
doRequestGetMethod req = case doRequestGetMethodImpl req of
  "POST" -> POST
  "GET" -> GET
  "DELETE" -> DELETE
  "PUT" -> PUT
  x -> Unknown x

foreign import doRequestGetBody :: DurableObjectRequest -> Aff String

-- private
foreign import doRequestGetMethodImpl :: DurableObjectRequest -> String

