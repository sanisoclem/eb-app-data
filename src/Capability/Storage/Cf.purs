module Capability.Storage.Cf where

import Prelude

import Capability.Has (class HasGetter, getter)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (class MonadState, gets)
import Data.Argonaut (Json)
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import FFI.DurableObject (DurableObjectState, doBatchState, doDeleteState, doGetState, doPutState, mkBatchedPut)

class Monad m <= MonadCfStorage m where
  tryGetState :: String -> m (Maybe Json)
  putState :: String -> Json -> m Unit
  deleteState :: String -> m Unit

instance monadCfStorageInstances :: (HasGetter DurableObjectState s, MonadState s m, MonadAff m, MonadThrow Error m) => MonadCfStorage m where
  tryGetState key = do
    state <- gets getter
    liftAff $ doGetState state key

  putState key value = do
    state <- gets getter
    liftAff <<< doPutState state key $ value
  deleteState key = liftAff <<< (flip doDeleteState) key =<< gets getter

class Monad m <= MonadCfStorageBatch m where
  runBatch :: { puts:: Array { docId :: String, body :: Json }, deletes :: Array String } -> m Unit

instance monadCfStorageBatchInstances :: (HasGetter DurableObjectState s, MonadState s m, MonadAff m) => MonadCfStorageBatch m where
  runBatch batch = do
    let puts = (mkBatchedPut <$> _.docId <*> _.body) <$> batch.puts
    state <- gets getter
    liftAff $ doBatchState state puts batch.deletes

-- | (->) :: Type -> Type -> Type
-- | _.docId :: Function batch docId
-- | _.body :: Function batch body
-- | mkBatchedPut :: Function docId (Function body putBatch) # docId -> body -> putBatch
-- | mkBatchedPut <$> _.docId :: Function batch (Function body putBatch)
-- | mkBatchedPut <$> _.docId <*> _.body :: Function batch putBatch # aka batch -> putBatch