module EB.DB.Capability.Outbox where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Argonaut (Json, decodeJson, encodeJson, stringify)
import Data.Array (cons)
import Data.Either (Either)
import Data.Maybe (fromMaybe)
import EB.DB.Capability.Storage.Transactional (class MonadTransactionalStorage, batchputDurableState, batchtryGetDurableState)
import EB.DB.Data.Utility (convertJsonErrorToError)
import Effect.Exception (Error)

class Monad m <= MonadOutbox e m where
  queue :: e -> m Unit

class OutboxEvent e where
  encodeEvent :: e -> Json
  decodeEvent :: Json -> Either Error e

outboxDocumentId :: String
outboxDocumentId = "outbox"

type OutboxDocument = Array String

instance (OutboxEvent e, MonadTransactionalStorage m, Monad m, MonadThrow Error m) => MonadOutbox e m where
  queue evt = do
    obJson <- batchtryGetDurableState outboxDocumentId
    (ob :: OutboxDocument) <- fromMaybe (pure []) (liftEither <<< convertJsonErrorToError <<< decodeJson <$> obJson)
    batchputDurableState outboxDocumentId $ encodeJson (cons (stringify <<< encodeEvent $ evt) ob)
    pure unit
else instance monadOutboxMonadTrans :: (Monad (t m), MonadOutbox e m, MonadTrans t) => MonadOutbox e (t m) where
  queue = lift <<< queue