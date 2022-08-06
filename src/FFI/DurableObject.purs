module FFI.DurableObject
  ( DurableObjectRequest
  , DurableObjectResponse
  , DurableObjectState
  , doRequestGetMethod
  , doStringResponse
  )
  where

import Data.Request (RequestMethod(..))

foreign import data DurableObjectRequest :: Type
foreign import data DurableObjectState :: Type
foreign import data DurableObjectResponse :: Type

foreign import doStringResponse :: String -> DurableObjectResponse

doRequestGetMethod :: DurableObjectRequest -> RequestMethod
doRequestGetMethod req = case doRequestGetMethodImpl req of
  "POST" -> POST
  "GET" -> GET
  "DELETE" -> DELETE
  "PUT" -> PUT
  x -> Unknown x

-- private
foreign import doRequestGetMethodImpl :: DurableObjectRequest -> String

