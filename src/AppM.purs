module EB.DB.AppM where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.State (class MonadState, StateT, runStateT)
import Data.Array (fromFoldable)
import Data.Lens (Iso', iso)
import Data.Lens.Record (prop)
import Data.Tuple (fst)
import EB.DB.Capability.Fetch (class MonadFetchRequest)
import EB.DB.Capability.Has (class HasLens, getState)
import EB.DB.Capability.Storage.Cf (class MonadCfStorage, class MonadCfStorageBatch)
import EB.DB.Data.Fetch (RequestMethod(..))
import EB.DB.FFI.DurableObject (DurableObjectRequest, DurableObjectState, doBatchState, dodeleteDurableState, dogetDurableState, dogetDurableStateByPrefix, doputDurableState, doRequestGetBody, doRequestGetMethod, doRequestGetParam, doRequestGetPath, mkBatchedPut)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Safe.Coerce (coerce)
import Type.Prelude (Proxy(..))

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
  tryGetDurableState key = do
    state <- getState
    liftAff $ dogetDurableState state key
  putDurableState key value = do
    state <- getState
    liftAff <<< doputDurableState state key $ value
  deleteDurableState key = liftAff <<< (flip dodeleteDurableState) key =<< getState
  getDurableStateByPrefix prefix = do
    state <- getState
    resultMap <- liftAff <<< dogetDurableStateByPrefix state $ prefix
    pure $ fromFoldable resultMap

instance MonadCfStorageBatch AppM where
  runBatch batch = do
    let puts = (mkBatchedPut <$> _.docId <*> _.body) <$> batch.puts
    state <- getState
    liftAff $ doBatchState state puts batch.deletes

instance MonadFetchRequest AppM where
  getRequestMethod = do
    request <- getState
    pure $ case doRequestGetMethod request of
      "POST" -> POST
      "GET" -> GET
      "DELETE" -> DELETE
      "PUT" -> PUT
      x -> Unknown x
  getBodyString = do
    request <- getState
    liftAff <<< doRequestGetBody $ request
  getPath = do
    req <- getState
    pure $ doRequestGetPath req
  tryGetParam key = do
    req <- getState
    pure $ doRequestGetParam req key

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

_ContextData :: Iso' ContextData { durableObjectRequest :: DurableObjectRequest, durableObjectState :: DurableObjectState }
_ContextData = iso (case _ of ContextData x -> x) ContextData

instance hasContextState :: HasLens ContextData DurableObjectState  where
  focus = _ContextData <<< prop (Proxy :: Proxy "durableObjectState")

instance hasContextRequest :: HasLens ContextData DurableObjectRequest where
  focus = _ContextData <<<  prop (Proxy :: Proxy "durableObjectRequest")
