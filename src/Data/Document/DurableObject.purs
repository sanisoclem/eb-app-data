module Data.Document.DurableObject where

import Prelude

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract, encodeContractJson)
import Capability.Storage (class DurableStorage, batchPutState, getState, putState)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (stringify)
import Data.Array (fromFoldable)
import Data.Contract.Document.DurableObject (OutboxContract(..))
import Data.Document.Id (outboxDocumentId)
import Data.Foldable (class Foldable)
import Effect.Exception (Error)

type OutboxDocumentRecord =
  { toSend :: Array String
  }

data OutboxDocument = OutboxDocument OutboxDocumentRecord

instance encodeDataContractOutboxDocument :: EncodeDataContract OutboxContract OutboxDocument where
  toContract (OutboxDocument x) = V1 x

instance decodeDataContractOutboxDocument :: DecodeDataContract OutboxContract OutboxDocument where
  fromContract (V1 x) = pure $ OutboxDocument x

getOutbox :: forall m. DurableStorage m => MonadThrow Error m => m OutboxDocumentRecord
getOutbox = do
  (OutboxDocument outbox) <- getState outboxDocumentId
  pure outbox

addToOutbox :: forall m evt' evt f. Foldable f => Functor f => DurableStorage m => MonadThrow Error m => EncodeDataContract evt' evt => f evt -> m Unit
addToOutbox evts = do
  outbox <- getOutbox
  let serialized = fromFoldable $ ((stringify <<< encodeContractJson) <$> evts)
  batchPutState outboxDocumentId <<< OutboxDocument $ outbox { toSend = outbox.toSend <> serialized }

clearOutbox
  :: forall m
   . DurableStorage m
  => m Unit
clearOutbox = putState outboxDocumentId $ OutboxDocument { toSend: [] }
