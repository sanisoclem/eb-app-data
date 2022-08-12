module Capability.Storage where

import Prelude

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract, decodeContractJson, encodeContractJson)
import Capability.Has (class Has, class HasSetter, getter, setter)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.State (class MonadState, gets, modify_)
import Data.Either (note)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)
import FFI.DurableObject (BatchedPut, DurableObjectState, doBatchState, doDeleteState, doGetState, doPutState, mkBatchedPut)

class Monad m <= DurableStorage m where
  tryGetState :: ∀ a b. (DecodeDataContract a b) => String -> m (Maybe b)
  putState :: ∀ a b. (EncodeDataContract a b) => String -> b -> m Unit
  deleteState :: String -> m Unit

  batchPutState :: ∀ a b. (EncodeDataContract a b) => String -> b -> m Unit
  batchDeleteState :: String -> m Unit
  commitBatchState :: m Unit

instance durableStorageInstance
  :: (HasSetter s (List BatchedPut)
   , HasSetter s (List String)
   , Has s (List String)
   , Has s (List BatchedPut)
   , Has s DurableObjectState
   , MonadState s m
   , MonadAff m
   , MonadThrow Error m)
  => DurableStorage m where
  tryGetState key = do
    state <- gets getter
    val <- liftAff $ doGetState state key
    sequence $ liftEither <<< decodeContractJson <$> val
  putState key value = do
    state <- gets getter
    liftAff <<< doPutState state key <<< encodeContractJson $ value
  deleteState key = liftAff <<< (flip doDeleteState) key =<< gets getter

  batchPutState id doc = modify_ <<< setter <<< (:) <<< mkBatchedPut id $ encodeContractJson doc
  batchDeleteState id = modify_ <<< setter <<< (:) $ id
  commitBatchState = do
    doBatchState
      <$> gets getter
      <*> gets getter
      <*> gets getter
      >>= liftAff
    modify_ <<< setter <<< const $ (Nil :: List String)
    modify_ <<< setter <<< const $ (Nil :: List BatchedPut)


getState :: ∀ a b m. (MonadThrow Error m) => (DurableStorage m) => (DecodeDataContract a b) => String -> m b
getState key = tryGetState key >>= note (error $ "state not found: " <> key) >>> liftEither
