module Capability.Storage.Database where

import Prelude

import Capability.Storage.Cf (class MonadCfStorage, getState, getStateByPrefix, tryGetState)
import Capability.Storage.Transactional (class MonadTransactionalStorage, batchDeleteState, batchGetState, batchPutState, batchTryGetState)
import Capability.Utility (convertJsonErrorToError)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Argonaut (Json, decodeJson, encodeJson)
import Data.Array (delete, elem, foldl, insert, singleton, union)
import Data.Either (Either, note)
import Data.Filterable (filterMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, alter, foldSubmap, intersectionWith)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Exception (Error, error)
import Type.Prelude (Proxy(..))

class DatabaseId dbId where
  dbIdString :: dbId -> String
  dbIdFromString :: String -> Maybe dbId

class DatabaseDocumentId dbId docId where
  wrapDocumentId :: docId -> dbId
  tryUnwrapDocumentId :: dbId -> Maybe docId

class DocumentCollection :: Type -> Constraint
class DocumentCollection doc where
  getIdPrefix :: Proxy doc -> String

class DocumentId :: Type -> Type -> Constraint
class DocumentId doc docId | doc -> docId

class DatabaseDocument doc where
  decodeDocument :: Json -> Either Error doc
  encodeDocument :: doc -> Json

class (Monad m, DatabaseId dbId) <= MonadReadonlyDatabase dbId m | m -> dbId where
  tryGetDocumentReadonly ::
    forall doc docId
     . DatabaseDocument doc
    => DocumentId doc docId
    => DatabaseDocumentId dbId docId
    => docId
    -> m (Maybe doc)
  getCollection ::
    forall doc docId
     . DatabaseDocument doc
    => DocumentId doc docId
    => DatabaseDocumentId dbId docId
    => DocumentCollection doc
    => m (Array doc)


class (Monad m, DatabaseId dbId) <= MonadDatabase dbId m | m -> dbId where
  tryGetDocument ::
    forall doc docId
     . DatabaseDocument doc
    => DocumentId doc docId
    => DatabaseDocumentId dbId docId
    => docId
    -> m (Maybe doc)
  putDocument ::
    forall doc docId
     . DatabaseDocument doc
    => DocumentId doc docId
    => DatabaseDocumentId dbId docId
    => docId
    -> doc
    -> m Unit
  deleteDocument ::
    forall docId
     . DatabaseDocumentId dbId docId
    => docId
    -> m Unit

class DatabaseIndex idx where
  getIndexId :: idx -> String

class Ord idx <= IndexedDocument doc idx | doc -> idx where
  getRangeIndexes :: doc -> Map idx Int

class MonadReadonlyIndexedDatabase :: Type -> Type -> (Type -> Type) -> Constraint
class (Monad m, DatabaseId dbId, DatabaseIndex idx) <= MonadReadonlyIndexedDatabase dbId idx m where
  getFromRangeIndexReadonly ::
    forall doc docId
     . DatabaseDocument doc
    => DatabaseDocumentId dbId docId
    => DocumentId doc docId
    => idx
    -> Maybe Int
    -> Maybe Int
    -> m (Array doc)

class (Monad m, DatabaseId dbId, DatabaseIndex idx) <= MonadIndexedDatabase dbId idx m where
  getFromRangeIndex ::
    forall doc docId
     . DatabaseDocument doc
    => DatabaseDocumentId dbId docId
    => DocumentId doc docId
    => idx
    -> Maybe Int
    -> Maybe Int
    -> m (Array doc)
  putIndexedDocument ::
    forall doc docId
     . IndexedDocument doc idx
    => DatabaseDocument doc
    => DatabaseDocumentId dbId docId
    => DocumentId doc docId
    => docId
    -> doc
    -> m Unit
  deleteIndexedDocument ::
    forall doc docId
     . IndexedDocument doc idx
    => DatabaseDocument doc
    => DatabaseDocumentId dbId docId
    => DocumentId doc docId
    => Proxy doc
    -> docId
    -> m Unit

type RangeIndexDocument = Map Int (Array String)

getDocumentReadonly ::
  forall dbId doc docId m
   . DatabaseId dbId
  => MonadThrow Error m
  => MonadReadonlyDatabase dbId m
  => DatabaseDocument doc
  => DatabaseDocumentId dbId docId
  => DocumentId doc docId
  => docId
  -> m doc
getDocumentReadonly = liftEither <<< note (error "document not found") <=< tryGetDocumentReadonly

getDocument ::
  forall dbId doc docId m
   . MonadThrow Error m
  => MonadDatabase dbId m
  => MonadTransactionalStorage m
  => DatabaseId dbId
  => DatabaseDocument doc
  => DocumentId doc docId
  => DatabaseDocumentId dbId docId
  => docId
  -> m doc
getDocument docId = do
  g <- tryGetDocument docId
  liftEither <<< note (error "document not found")  $ g

getFullIndexId ::
  forall idx
  . DatabaseIndex idx
  => idx
  -> String
getFullIndexId idx = "idx/" <> getIndexId idx

getFullDbStringId ::
  forall dbId
   . DatabaseId dbId
  => dbId
  -> String
getFullDbStringId dbId = "d/" <> dbIdString dbId

instance (MonadCfStorage m, MonadThrow Error m, DatabaseId dbId) => MonadReadonlyDatabase dbId m where
  tryGetDocumentReadonly id = do
    let (dbId :: dbId) =  wrapDocumentId id
    jsonState <- tryGetState <<< getFullDbStringId $ dbId
    sequence $ (liftEither <<< decodeDocument) <$> jsonState
  getCollection ::
    forall doc docId
     . DatabaseDocument doc
    => DocumentId doc docId
    => DatabaseDocumentId dbId docId
    => DocumentCollection doc
    => m (Array doc)
  getCollection = do
    let prefix = getIdPrefix (Proxy :: Proxy doc)
    jsons <- getStateByPrefix prefix
    sequence $ (liftEither <<< decodeDocument) <$> jsons

instance (MonadTransactionalStorage m, MonadThrow Error m, DatabaseId dbId) => MonadDatabase dbId m where
  tryGetDocument id = do
    let (dbId :: dbId) =  wrapDocumentId id
    jsonState <- batchTryGetState <<< getFullDbStringId $ dbId
    sequence $ (liftEither <<< decodeDocument) <$> jsonState
  putDocument docId doc = do
    let (dbId :: dbId) = wrapDocumentId docId
    batchPutState (getFullDbStringId dbId) $ encodeDocument doc
  deleteDocument docId = do
    let (dbId :: dbId) = wrapDocumentId docId
    batchDeleteState $ getFullDbStringId dbId

instance (MonadCfStorage m, MonadThrow Error m, DatabaseId dbId, DatabaseIndex idx, MonadReadonlyDatabase dbId m) => MonadReadonlyIndexedDatabase dbId idx m where
  getFromRangeIndexReadonly index min max = do
    (indexDoc :: RangeIndexDocument) <- liftEither <=< map (convertJsonErrorToError <<< decodeJson) <<< getState $ getFullIndexId index
    let (ids :: Array String) = foldSubmap min max (\_k v -> v) indexDoc
    let (dbIds :: Array dbId) = filterMap dbIdFromString ids
    sequence $ getDocumentReadonly <$> filterMap tryUnwrapDocumentId dbIds


instance (MonadTransactionalStorage m, MonadThrow Error m, DatabaseId dbId, DatabaseIndex idx, MonadDatabase dbId m) => MonadIndexedDatabase dbId idx m where
  getFromRangeIndex :: forall doc docId. DatabaseDocument doc => DatabaseDocumentId dbId docId => DocumentId doc docId => idx -> Maybe Int -> Maybe Int -> m (Array doc)
  getFromRangeIndex index min max = do
    (indexDoc :: RangeIndexDocument) <- liftEither <=< map (convertJsonErrorToError <<< decodeJson) <<< batchGetState $ getFullIndexId index
    let (ids :: Array String) = foldSubmap min max (\_k v -> v) indexDoc
    let (dbIds :: Array dbId) = filterMap dbIdFromString ids
    let (docIds :: Array docId) = filterMap tryUnwrapDocumentId dbIds
    sequence $ getDocument <$> docIds
  putIndexedDocument :: forall doc docId. IndexedDocument doc idx => DatabaseDocument doc => DatabaseDocumentId dbId docId => DocumentId doc docId => docId -> doc -> m Unit
  putIndexedDocument dId newDoc = do
    (prevDoc :: Maybe doc) <- tryGetDocument dId
    updateIndexes prevDoc newDoc
    putDocument dId newDoc
      where
        updateIndexes :: Maybe doc -> doc -> m Unit
        updateIndexes prevDoc doc = do
          let prevIndexes = (map (singleton <<< { delete: true, value: _ }) <<< getRangeIndexes) <$> prevDoc
          let indexes = singleton <<< { delete: false, value: _ } <$> getRangeIndexes doc
          let allUpdates = case prevIndexes of
                Just x -> intersectionWith union x indexes
                Nothing -> indexes
          let (dbId :: dbId) = wrapDocumentId dId
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
  deleteIndexedDocument :: forall doc docId. IndexedDocument doc idx => DatabaseDocument doc => DocumentId doc docId => DatabaseDocumentId dbId docId => Proxy doc -> docId -> m Unit
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

