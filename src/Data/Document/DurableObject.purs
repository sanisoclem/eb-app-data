module Data.Document.DurableObject where

import Prelude

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract, encodeContractJson, generateId, toDocumentId)
import Capability.Storage (class DurableStorage, batchPutState, getState, putState)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (class DecodeJson, class EncodeJson, stringify)
import Data.Argonaut.Decode.Class (class DecodeJsonField)
import Data.Array (fromFoldable)
import Data.Common (IndexId(..), IndexPageId(..), mkIndexId)
import Data.Contract.Document.DurableObject (IndexContract(..), IndexPageContract(..), OutboxContract(..))
import Data.Document.Id (outboxDocumentId)
import Data.Foldable (class Foldable)
import Data.Map (Map, empty, lookupLE)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)

type OutboxDocumentRecord =
  { toSend :: Array String
  }

data OutboxDocument = OutboxDocument OutboxDocumentRecord

instance encodeDataContractOutboxDocument :: EncodeDataContract OutboxContract OutboxDocument where
  toContract (OutboxDocument x) = OutboxContractV1 x

instance decodeDataContractOutboxDocument :: DecodeDataContract OutboxContract OutboxDocument where
  fromContract (OutboxContractV1 x) = pure $ OutboxDocument x

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

data IndexDocument a = IndexDocument
  { indexId :: IndexId
  , pageSize :: Int
  , pages :: Map a IndexPageId
  }
instance encodeDataContractIndexDocument :: (EncodeJson a, Ord a) => EncodeDataContract (IndexContract a) (IndexDocument a) where
  toContract (IndexDocument x) = IndexContractV1 x

instance decodeDataContractIndexDocument :: (DecodeJson a, Ord a) => DecodeDataContract (IndexContract a) (IndexDocument a) where
  fromContract (IndexContractV1 x) = pure $ IndexDocument x

data IndexPageDocument a = IndexPageDocument
  { pageId :: IndexPageId
  , docIds :: Array (IndexEntry a)
  }
data IndexEntry a = IndexEntry a String

instance eqIndexEntry :: Eq a => Eq (IndexEntry a) where
  eq (IndexEntry a _) (IndexEntry b _) = eq a b

instance ordIndexEntry :: Ord a => Ord (IndexEntry a) where
  compare (IndexEntry a _) (IndexEntry b _) = compare a b

instance encodeDataContractIndexPageDocument :: (EncodeJson a, Ord a) => EncodeDataContract (IndexPageContract a) (IndexPageDocument a) where
  toContract (IndexPageDocument x) = IndexPageContractV1 x

instance decodeDataContractIndexPageDocument :: (DecodeJsonField a, Ord a) => DecodeDataContract (IndexPageContract a) (IndexPageDocument a) where
  fromContract (IndexPageContractV1 x) = pure $ IndexPageDocument x

createIndex :: forall a m. DurableStorage m => EncodeJson a => Ord a => String -> Int -> m (IndexDocument a)
createIndex name pageSize = do
  let indexId = mkIndexId name
  let idx = IndexDocument { indexId, pageSize, pages: empty }
  batchPutState (toDocumentId indexId) idx
  pure idx

-- caveat: does not work when there are pending changes for this index
addToIndex :: forall a m. DurableStorage m => MonadThrow Error m => DecodeJson a => EncodeJson a => Ord a => IndexId -> a -> String -> m Unit
addToIndex indexId val docId = do
  index <- getState indexId
  page <- getIndexPage index val
  let { updatedPages, updatedIndex } = addToPage index page val docId
  batchPutState (toDocumentId updatedIndex.indexId) updatedIndex
  sequence $ (\p -> batchPutState (toDocumentId p.pageId) p) <$> updatedPages

-- queryRange :: forall a m. DurableStorage m => a -> a -> Array String
-- queryRange minInc maxInc = do
--   ?todo


getIndexPage :: forall a m. MonadEffect m => DurableStorage m => Ord a => DecodeJsonField a => MonadThrow Error m => IndexDocument a -> a -> m (IndexPageDocument a)
getIndexPage (IndexDocument index) val = case lookupLE val index.pages of
  Just page -> getState (toDocumentId page.value)
  Nothing -> do
    pageId <- generateId
    let newPage = IndexPageDocument { pageId
      , docIds: []
      }
    pure newPage

addToPage :: forall a. IndexDocument a -> IndexPageDocument a -> a -> String -> { updatedPages :: Array (IndexPageDocument a), updatedIndex :: IndexDocument a }
addToPage index page val docId = do

  ?todo