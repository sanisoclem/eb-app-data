module AppM where

import Prelude

import Capability.Fetch (class MonadFetchRequest)
import Capability.Has (class HasGetter, getter)
import Capability.Now (class MonadNow)
import Capability.Storage.Cf (class MonadCfStorage, class MonadCfStorageBatch, getStateByPrefix)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.State (class MonadState, StateT, gets, runStateT)
import Data.Array (fromFoldable)
import Data.DateTime.Instant as StdInstant
import Data.Fetch (RequestMethod(..))
import Data.Instant (mkInstant)
import Data.Int (ceil)
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (now)
import FFI.DurableObject (DurableObjectRequest, DurableObjectState, doBatchState, doDeleteState, doGetState, doGetStateByPrefix, doPutState, doRequestGetBody, doRequestGetMethod, doRequestGetParam, doRequestGetPath, mkBatchedPut)
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

instance MonadCfStorage AppM where
  tryGetState key = do
    state <- gets getter
    liftAff $ doGetState state key
  putState key value = do
    state <- gets getter
    liftAff <<< doPutState state key $ value
  deleteState key = liftAff <<< (flip doDeleteState) key =<< gets getter
  getStateByPrefix prefix = do
    state <- gets getter
    resultMap <- liftAff <<< doGetStateByPrefix state $ prefix
    pure $ fromFoldable resultMap

instance MonadCfStorageBatch AppM where
  runBatch batch = do
    let puts = (mkBatchedPut <$> _.docId <*> _.body) <$> batch.puts
    state <- gets getter
    liftAff $ doBatchState state puts batch.deletes

instance incomingRequestAppM :: MonadFetchRequest AppM where
  getRequestMethod = do
    request <- gets getter
    pure $ case doRequestGetMethod request of
      "POST" -> POST
      "GET" -> GET
      "DELETE" -> DELETE
      "PUT" -> PUT
      x -> Unknown x
  getBodyString = do
    request <- gets getter
    liftAff <<< doRequestGetBody $ request
  getPath = do
    req <- gets getter
    pure $ doRequestGetPath req
  tryGetParam key = do
    req <- gets getter
    pure $ doRequestGetParam req key

instance MonadNow AppM where
  nowUtc = do
    inst <- liftEffect now
    pure <<< mkInstant <<< ceil <<< unwrap <<< StdInstant.unInstant $ inst

data ContextData = ContextData
  { durableObjectRequest :: DurableObjectRequest
  , durableObjectState :: DurableObjectState
  }

mkContext :: DurableObjectState -> DurableObjectRequest -> ContextData
mkContext durableObjectState durableObjectRequest =
  ContextData
    { durableObjectRequest
    , durableObjectState
    }

instance hasContextState :: HasGetter DurableObjectState ContextData  where
  getter (ContextData x) = x.durableObjectState

instance hasContextRequest :: HasGetter DurableObjectRequest ContextData  where
  getter (ContextData x) = x.durableObjectRequest
