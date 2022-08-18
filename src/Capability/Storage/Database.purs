module Capability.Storage.Database where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Capability.Storage.Transactional (class MonadTransactionalStorage, batchDeleteState, batchGetState, batchPutState, batchTryGetState)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson, printJsonDecodeError)
import Data.Array (delete, elem, foldl, insert, singleton, union)
import Data.Bifunctor (lmap)
import Data.Either (Either, hush, note)
import Data.Filterable (filterMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, alter, foldSubmap, intersectionWith)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Exception (Error, error)
import Type.Proxy (Proxy)

class DatabaseId dbId where
  dbIdString :: dbId -> String
  dbIdFromString :: String -> Maybe dbId

class DocumentId dbId docId where
  wrapDocumentId :: docId -> dbId
  tryUnwrapDocumentId :: dbId -> Maybe docId

class DatabaseDocument doc docId | doc -> docId where
  getDocumentId :: doc -> docId
  decode :: Json -> Either JsonDecodeError doc
  encode :: doc -> Json

class (Monad m, DatabaseId dbId) <= MonadDatabase dbId m | m -> dbId where
  tryGetDocument :: forall doc docId. DatabaseDocument doc docId => DocumentId dbId docId => docId -> m (Maybe doc)
  putDocument :: forall doc docId. DatabaseDocument doc docId => DocumentId dbId docId => doc -> m Unit
  deleteDocument :: forall docId. DocumentId dbId docId => docId -> m Unit

class DatabaseIndex idx where
  getIndexId :: idx -> String

class Ord idx <= IndexedDocument doc idx | doc -> idx where
  getRangeIndexes :: doc -> Map idx Int

class (Monad m, DatabaseId dbId, DatabaseIndex idx) <= MonadIndexedDatabase dbId idx m where
  getFromRangeIndex :: forall doc docId. DatabaseDocument doc docId => DocumentId dbId docId => idx -> Maybe Int -> Maybe Int -> m (Array doc)
  putIndexedDocument :: forall doc docId. IndexedDocument doc idx => DatabaseDocument doc docId => DocumentId dbId docId => doc -> m Unit
  deleteIndexedDocument :: forall doc docId. IndexedDocument doc idx => DatabaseDocument doc docId => DocumentId dbId docId => Proxy doc -> docId -> m Unit

type RangeIndexDocument = Map Int (Array String)

getDocument :: forall dbId doc docId m. DatabaseId dbId => MonadThrow Error m => MonadDatabase dbId m => DatabaseDocument doc docId => DocumentId dbId docId => docId -> m doc
getDocument = liftEither <<< note (error "document not found") <=< tryGetDocument

convertJsonErrorToError :: forall a. Either JsonDecodeError a -> Either Error a
convertJsonErrorToError = lmap (error <<< printJsonDecodeError)

getFullIndexId :: forall idx. DatabaseIndex idx => idx -> String
getFullIndexId idx = "idx/" <> getIndexId idx

getFullDbStringId :: forall dbId. DatabaseId dbId => dbId -> String
getFullDbStringId dbId = "d/" <> dbIdString dbId

instance monadDatabaseInstances :: (MonadTransactionalStorage m, MonadThrow Error m, DatabaseId dbId) => MonadDatabase dbId m where
  tryGetDocument id = do
    let (dbId :: dbId) =  wrapDocumentId id
    jsonState <- batchTryGetState <<< getFullDbStringId $ dbId
    pure $ hush <<< decode =<< jsonState
  putDocument doc = do
    let (dbId :: dbId) = wrapDocumentId $ getDocumentId doc
    batchPutState (getFullDbStringId dbId) $ encode doc
  deleteDocument docId = do
    let (dbId :: dbId) = wrapDocumentId docId
    batchDeleteState $ getFullDbStringId dbId


test :: forall t196 t197.
  IndexedDocument t196 t197 => t196
                               -> Map t197
                                    (Array
                                       { delete :: Boolean
                                       , value :: Int
                                       }
                                    )
test = map (singleton <<< { delete: true, value: _ }) <<< getRangeIndexes

instance monadIndexedDatabaseInstances :: (MonadTransactionalStorage m, MonadThrow Error m, DatabaseId dbId, DatabaseIndex idx, MonadDatabase dbId m) => MonadIndexedDatabase dbId idx m where
  getFromRangeIndex index min max = do
    (indexDoc :: RangeIndexDocument) <- liftEither <=< map (convertJsonErrorToError <<< decodeJson) <<< batchGetState $ getFullIndexId index
    let (ids :: Array String) = foldSubmap min max (\_k v -> v) indexDoc
    let (dbIds :: Array dbId) = filterMap dbIdFromString ids
    sequence $ getDocument <$> filterMap tryUnwrapDocumentId dbIds
  putIndexedDocument :: forall doc docId. IndexedDocument doc idx => DatabaseDocument doc docId => DocumentId dbId docId => doc -> m Unit
  putIndexedDocument newDoc = do
    (prevDoc :: Maybe doc) <- tryGetDocument <<< getDocumentId $ newDoc
    updateIndexes prevDoc newDoc
    putDocument newDoc
      where
        updateIndexes :: Maybe doc -> doc -> m Unit
        updateIndexes prevDoc doc = do
          let prevIndexes = (map (singleton <<< { delete: true, value: _ }) <<< getRangeIndexes) <$> prevDoc
          let indexes = singleton <<< { delete: false, value: _ } <$> getRangeIndexes doc
          let allUpdates = case prevIndexes of
                Just x -> intersectionWith union x indexes
                Nothing -> indexes
          let (docId :: docId) = getDocumentId doc
          let (dbId :: dbId) = wrapDocumentId docId
          let id = dbIdString dbId
          void <$> sequence $ mapWithIndex (updateIndex id) allUpdates
        updateIndex :: String -> idx -> Array { delete :: Boolean, value :: Int } -> m Unit
        updateIndex docId idx updates = do
          let indexId = getFullIndexId idx
          (indexDoc :: RangeIndexDocument) <- liftEither <=< map (convertJsonErrorToError <<< decodeJson) <<< batchGetState $ indexId
          let updatedDoc = foldl (applyUpdates docId) indexDoc updates
          batchPutState indexId (encodeJson updatedDoc)
        applyUpdates docId index = case _ of
          { delete: true, value: x } -> alter (removeAndDeleteIfEmpty docId) x index
          { delete: false, value: x } -> alter (insertOrUpdate docId) x index
        removeAndDeleteIfEmpty docId = case _ of
          Nothing -> Nothing
          Just [x] | eq docId x -> Nothing
          Just x -> Just $ delete docId x
        insertOrUpdate docId = case _ of
          Nothing -> Just [docId]
          Just x | elem docId x -> Just x
          Just x -> Just $ insert docId x
  deleteIndexedDocument :: forall doc docId. IndexedDocument doc idx => DatabaseDocument doc docId => DocumentId dbId docId => Proxy doc -> docId -> m Unit
  deleteIndexedDocument _ docId = do
    (d :: Maybe doc) <- tryGetDocument docId
    case d of
      Just doc -> do
        let (dbId :: dbId) = wrapDocumentId docId
        let id = dbIdString dbId
        let indexes = getRangeIndexes doc
        void <$> sequence $ mapWithIndex (removeFromIndex id) indexes
          where
            removeFromIndex idToRemove idx v = do
              let indexId = getFullIndexId idx
              (indexDoc :: RangeIndexDocument) <- liftEither <=< map (convertJsonErrorToError <<< decodeJson) <<< batchGetState $ indexId
              let updatedDoc = alter (removeAndDeleteIfEmpty idToRemove) v indexDoc
              batchPutState indexId (encodeJson updatedDoc)
            removeAndDeleteIfEmpty id = case _ of
              Nothing -> Nothing
              Just [x] | eq id x -> Nothing
              Just x -> Just $ delete id x
      Nothing -> pure unit

