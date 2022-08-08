module FFI.DurableObject
  ( DurableObjectRequest
  , DurableObjectResponse
  , DurableObjectState
  , doRequestGetMethod
  , doStringResponse
  , doRequestGetBody
  , doGetState
  , doPutState
  , doDeleteState
  , doBatchState
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(..))
import Data.Request (RequestMethod(..))
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data DurableObjectRequest :: Type
foreign import data DurableObjectState :: Type
foreign import data DurableObjectResponse :: Type

foreign import doStringResponse :: String -> Int -> DurableObjectResponse

type BatchedPut =
  { key :: String
  , value :: Json
  }

doRequestGetMethod :: DurableObjectRequest -> RequestMethod
doRequestGetMethod req = case doRequestGetMethodImpl req of
  "POST" -> POST
  "GET" -> GET
  "DELETE" -> DELETE
  "PUT" -> PUT
  x -> Unknown x

doRequestGetBody :: DurableObjectRequest -> Aff String
doRequestGetBody = doRequestGetBodyImpl >>> toAffE

doGetState :: DurableObjectState -> String -> Aff (Maybe Json)
doGetState state = doGetStateImpl Just Nothing state >>> toAffE

doPutState :: DurableObjectState -> String -> Json -> Aff Unit
doPutState state key = doPutStateImpl state key >>> toAffE

doDeleteState :: DurableObjectState -> String -> Aff Unit
doDeleteState state = doDeleteStateImpl state >>> toAffE

doBatchState :: DurableObjectState -> Array String -> Array BatchedPut -> Aff Unit
doBatchState state deletes = doBatchStateImpl state deletes >>> toAffE

-- private
foreign import doRequestGetBodyImpl :: DurableObjectRequest -> Effect (Promise String)
foreign import doRequestGetMethodImpl :: DurableObjectRequest -> String
foreign import doGetStateImpl :: (∀ a. a -> Maybe a) -> (∀ a. Maybe a) -> DurableObjectState -> String -> Effect (Promise (Maybe Json))
foreign import doPutStateImpl :: DurableObjectState -> String -> Json -> Effect (Promise Unit)
foreign import doDeleteStateImpl :: DurableObjectState -> String -> Effect (Promise Unit)
foreign import doBatchStateImpl :: DurableObjectState -> Array String -> Array BatchedPut -> Effect (Promise Unit)
