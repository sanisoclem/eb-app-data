module EB.DB.FFI.DurableObject
  ( BatchedPut
  , DurableObjectRequest
  , DurableObjectResponse
  , DurableObjectState
  , doBatchState
  , doDeleteState
  , doGetState
  , doGetStateByPrefix
  , doPutState
  , doRequestGetBody
  , doRequestGetMethod
  , doRequestGetParam
  , doRequestGetPath
  , doStringResponse
  , mkBatchedPut
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json)
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data DurableObjectRequest :: Type
foreign import data DurableObjectState :: Type
foreign import data DurableObjectResponse :: Type

foreign import doStringResponse :: String -> Int -> DurableObjectResponse
foreign import testReq :: String -> DurableObjectRequest
foreign import testState :: Unit -> DurableObjectState

newtype BatchedPut = BatchedPut
  { id :: String
  , document :: Json
  }
mkBatchedPut ∷ String → Json → BatchedPut
mkBatchedPut id document = BatchedPut { id, document }

foreign import doRequestGetMethod :: DurableObjectRequest -> String

foreign import doRequestGetPath :: DurableObjectRequest -> String

doRequestGetParam :: DurableObjectRequest -> String -> Maybe String
doRequestGetParam = doRequestGetParamImpl Just Nothing

doRequestGetBody :: DurableObjectRequest -> Aff String
doRequestGetBody = doRequestGetBodyImpl >>> toAffE

doGetStateByPrefix :: DurableObjectState -> String -> Aff (Map String Json)
doGetStateByPrefix state prefix = do
  items <- toAffE <<< doGetStateByPrefixImpl state $ prefix
  pure $ Map.fromFoldable $ (\r -> Tuple r.key r.value) <$> items

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
foreign import doGetStateByPrefixImpl :: DurableObjectState -> String -> Effect (Promise (Array { key :: String, value :: Json}))
foreign import doGetStateImpl :: (∀ a. a -> Maybe a) -> (∀ a. Maybe a) -> DurableObjectState -> String -> Effect (Promise (Maybe Json))
foreign import doPutStateImpl :: DurableObjectState -> String -> Json -> Effect (Promise Unit)
foreign import doDeleteStateImpl :: DurableObjectState -> String -> Effect (Promise Unit)
foreign import doBatchStateImpl :: DurableObjectState -> Array String -> Array BatchedPut -> Effect (Promise Unit)
foreign import doRequestGetParamImpl :: (∀ a. a -> Maybe a) -> (∀ a. Maybe a) -> DurableObjectRequest -> String -> Maybe String
