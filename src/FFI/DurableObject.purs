module FFI.DurableObject
  ( BatchedPut
  , DurableObjectRequest
  , DurableObjectResponse
  , DurableObjectState
  , doBatchState
  , doDeleteState
  , doGetState
  , doPutState
  , doRequestGetBody
  , doRequestGetMethod
  , doStringResponse
  , mkBatchedPut
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json)
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Request (RequestMethod(..))
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data DurableObjectRequest :: Type
foreign import data DurableObjectState :: Type
foreign import data DurableObjectResponse :: Type

foreign import doStringResponse :: String -> Int -> DurableObjectResponse

newtype BatchedPut = BatchedPut
  { id :: String
  , document :: Json
  }
mkBatchedPut ∷ String → Json → BatchedPut
mkBatchedPut id document = BatchedPut { id, document }

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

doBatchState :: ∀ f f'. Foldable f => Foldable f' => DurableObjectState -> f BatchedPut -> f' String -> Aff Unit
doBatchState state puts deletes = toAffE $ doBatchStateImpl state (fromFoldable deletes) (fromFoldable puts)

-- private
foreign import doRequestGetBodyImpl :: DurableObjectRequest -> Effect (Promise String)
foreign import doRequestGetMethodImpl :: DurableObjectRequest -> String
foreign import doGetStateImpl :: (∀ a. a -> Maybe a) -> (∀ a. Maybe a) -> DurableObjectState -> String -> Effect (Promise (Maybe Json))
foreign import doPutStateImpl :: DurableObjectState -> String -> Json -> Effect (Promise Unit)
foreign import doDeleteStateImpl :: DurableObjectState -> String -> Effect (Promise Unit)
foreign import doBatchStateImpl :: DurableObjectState -> Array String -> Array BatchedPut -> Effect (Promise Unit)
