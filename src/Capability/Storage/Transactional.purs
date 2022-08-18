module Capability.Storage.Transactional where

import Prelude

import Capability.Storage.Cf (class MonadCfStorage, class MonadCfStorageBatch, runBatch, tryGetState)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Argonaut (Json)
import Data.Array (elem, find, (:))
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error)
import Safe.Coerce (coerce)

type PutOperation = { docId :: String, body :: Json }

newtype BatchOperation = BatchOperation { puts:: Array PutOperation, deletes :: Array String }

newtype TransactionalStorageT m a = TransactionalStorageT (StateT BatchOperation m a)
derive newtype instance functorTransactionalStorageT :: Functor m => Functor (TransactionalStorageT m)
derive newtype instance applyTransactionalStorageT :: Monad m =>  Apply (TransactionalStorageT m)
derive newtype instance applicativeTransactionalStorageT :: Monad m => Applicative (TransactionalStorageT m)
derive newtype instance bindTransactionalStorageT :: Monad m => Bind (TransactionalStorageT m)
derive newtype instance monadTransactionalStorageT :: Monad m => Monad (TransactionalStorageT m)
derive newtype instance monadThrowDurableStorageT :: MonadThrow e m => MonadThrow e (TransactionalStorageT m)
derive newtype instance monadStateTransactionalStorageT :: Monad m => MonadState BatchOperation (TransactionalStorageT m)
derive newtype instance monadTransTransactionalStorageT :: MonadTrans TransactionalStorageT
derive newtype instance monadEffectTransactionalStorageT :: (MonadEffect m) => MonadEffect (TransactionalStorageT m)
derive newtype instance monadAffTransactionalStorageT :: (MonadAff m) => MonadAff (TransactionalStorageT m)

batchOperation :: ∀ m a. MonadCfStorageBatch m => TransactionalStorageT m a -> m a
batchOperation m = do
  (Tuple a batch) <- runStateT (coerce m) (BatchOperation { puts: [], deletes: []})
  runBatch $ coerce batch
  pure a

-- TODO: error when delete + put in the same batch
batchPutState' :: ∀ m. Monad m => String -> Json -> TransactionalStorageT m Unit
batchPutState' id doc = modify_ <<< setter $ (:) { docId: id, body: doc }
  where
    setter fn (BatchOperation x) = BatchOperation x { puts = fn x.puts }

batchDeleteState' :: ∀ m. Monad m => String -> TransactionalStorageT m Unit
batchDeleteState' id = modify_ <<< setter $ (:) id
  where
    setter fn (BatchOperation x) = BatchOperation x { deletes = fn x.deletes }

batchTryGetState' :: ∀ m. MonadCfStorage m => String -> TransactionalStorageT m (Maybe Json)
batchTryGetState' key = do
  (BatchOperation batch) <- get
  let isDeleted = elem key batch.deletes
  let lastUpdate = find ((==) key <<< _.docId) batch.puts
  case { isDeleted, lastUpdate } of
    { isDeleted: true, lastUpdate: _ } -> pure Nothing
    { isDeleted:_, lastUpdate: Just x } -> pure $ Just x.body
    _ -> lift $ tryGetState key

class Monad m <= MonadTransactionalStorage m where
  batchTryGetState :: String -> m (Maybe Json)
  batchPutState :: String -> Json -> m Unit
  batchDeleteState :: String -> m Unit

instance monadTransactionalStorageTransactionalStorageT :: (Monad m, MonadCfStorage m) => MonadTransactionalStorage (TransactionalStorageT m) where
  batchTryGetState = batchTryGetState'
  batchPutState = batchPutState'
  batchDeleteState = batchDeleteState'


batchGetState :: ∀ m. (MonadThrow Error m) => (MonadTransactionalStorage m) => String -> m Json
batchGetState key = batchTryGetState key >>= note (error $ "state not found: " <> key) >>> liftEither
