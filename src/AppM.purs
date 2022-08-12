module AppM where

import Prelude

import Capability.DataContract (decodeContractJson, encodeContractJson)
import Capability.Has (class Has, class HasSetter, getter, setter)
import Capability.IncomingRequest (class IncomingRequest)
import Capability.Storage (class DurableStorage)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, liftEither)
import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.List (List(..), (:))
import Data.Traversable (sequence)
import Data.Tuple (fst)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import FFI.DurableObject (BatchedPut, DurableObjectRequest, DurableObjectState, doBatchState, doDeleteState, doGetState, doPutState, doRequestGetBody, doRequestGetMethod, mkBatchedPut)
import Safe.Coerce (coerce)

newtype AppM a = AppM (StateT ContextData Aff a)

runAppM :: ∀ a. ContextData -> AppM a -> Aff a
runAppM state = map fst <<< (runStateT <@> state) <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadThrowAppM :: MonadThrow Error AppM
derive newtype instance monadErrorAppM :: MonadError Error AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadState ContextData AppM

instance durableStorageAppM :: DurableStorage AppM where
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

instance incomingRequestAppM :: IncomingRequest AppM where
  getRequestMethod = do
    request <- gets getter
    pure <<< doRequestGetMethod $ request
  getBodyString = do
    request <- gets getter
    liftAff <<< doRequestGetBody $ request

data ContextData = ContextData
  { durableObjectRequest :: DurableObjectRequest
  , durableObjectState :: DurableObjectState
  , batchPuts :: List BatchedPut
  , batchDeletes :: List String
  }

mkContext :: DurableObjectState -> DurableObjectRequest -> ContextData
mkContext durableObjectState durableObjectRequest =
  ContextData
    { durableObjectRequest
    , durableObjectState
    , batchPuts: Nil
    , batchDeletes: Nil
    }

instance hasContextState :: Has ContextData DurableObjectState where
  getter (ContextData x) = x.durableObjectState

instance hasContextRequest :: Has ContextData DurableObjectRequest where
  getter (ContextData x) = x.durableObjectRequest

instance hasContextBatchPutsGetter :: Has ContextData (List BatchedPut) where
  getter (ContextData x) = x.batchPuts

instance hasContextBatchPutsSetter :: HasSetter ContextData (List BatchedPut) where
  setter fn (ContextData x) = ContextData x { batchPuts = fn x.batchPuts }

instance hasContextBatchDeletesGetter :: Has ContextData (List String) where
  getter (ContextData x) = x.batchDeletes

instance hasContextBatchDeletesSetter :: HasSetter ContextData (List String) where
  setter fn (ContextData x) = ContextData x { batchDeletes = fn x.batchDeletes }
