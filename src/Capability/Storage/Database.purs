module Capability.Storage.Database where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Exception (Error, error)



-- getDocument :: forall m a docId schema. MonadThrow Error m => MonadTransactionalStorage m => Decodable a => DocumentId docId => docId -> m a
-- getDocument docId = do
--   liftEither <=< map decode <<< batchGetState <<< dbIdString $ docId

class DatabaseId dbId where
  dbIdString :: dbId -> String

class DocumentId dbId docId where
  wrapDocumentId :: docId -> dbId

class DatabaseDocument doc docId | doc -> docId where
  getDocumentId :: doc -> docId
  decode :: Json -> Either JsonDecodeError doc
  encode :: doc -> Json

class (Monad m, DatabaseId dbId) <= MonadDatabase dbId m | m -> dbId where
  getDocument :: forall doc docId. DatabaseDocument doc docId => DocumentId dbId docId => docId -> m doc
  putDocument :: forall doc docId. DatabaseDocument doc docId => DocumentId dbId docId => doc -> m Unit
  deleteDocument :: forall docId. DocumentId dbId docId => docId -> m Unit

-- instance testInstance :: (MonadTransactionalStorage m, MonadThrow Error m, DatabaseId dbId) => MonadDatabase dbId m where
--   getDocument id = do
--     let (dbId :: dbId) =  wrapDocumentId id
--     jsonState <- batchGetState <<< ((<>) "d/") <<< dbIdString $ dbId
--     liftEither <<< convertJsonErrorToError $ decode jsonState
--   putDocument doc = do
--     let (dbId :: dbId) = wrapDocumentId $ getDocumentId doc
--     batchPutState (dbIdString dbId) $ encode doc
--   deleteDocument docId = do
--     let (dbId :: dbId) = wrapDocumentId docId
--     batchDeleteState $ dbIdString dbId

convertJsonErrorToError :: forall a. Either JsonDecodeError a -> Either Error a
convertJsonErrorToError = lmap (error <<< printJsonDecodeError)

  -- getFromIndex index min max = do
  --   (indexDoc :: IndexPage) <- map decodeJson <<< batchGetState $ getIndexId index
  --   ?todo

-- class DatabaseIndex idx where
--   getIndexId :: idx -> String

-- class DatabaseIndexValue idxv  where
--   encodeIndexValue :: idxv -> Json
--   decodeIndexValue :: forall v. Ord v => Json -> v

-- class (Monad m, DatabaseId docId, DatabaseIndex idx, DatabaseIndexValue idxv) <= MonadIndexedDatabase docId idx idxv m where
--   getFromIndex :: forall doc. DatabaseDocument doc docId => idx -> Maybe idxv -> Maybe idxv -> m (Array doc)

-- instance testIndexedInstance :: (MonadTransactionalStorage m, MonadThrow Error m, DatabaseId docId) => MonadIndexedDatabase docId m where
--   getFromIndex index min max = do
--     (indexDoc :: IndexPage) <- map decodeJson <<< batchGetState $ getIndexId index


-- type IndexPage = Map Json (Array String)


-- class (Monad m, DocumentId docId schema) <= MonadDatabase schema docId idx idxv m | schema -> docId, schema -> idx, idx -> idxv where
--   getDocument :: forall a. SchemaDocument a schema => DocumentId docId schema => docId -> m a
--   getFromIndex :: forall a. DocumentIndex idx idxv schema => idx -> Maybe idxv -> Maybe idxv -> m (Array a)
  -- putUniqueDocument :: forall a. UniqueDocument a => a -> m Unit
  -- putDocument :: forall a. DocumentCollection a => a -> m Unit

--getFromIndex :: forall a b v. DocumentIndex a b v => b -> Maybe v -> Maybe v -> m (Array a)

-- class DocumentCollection d where
--   getDocumentId :: d -> DocumentId

-- class DocumentIndex d idx idxv | idx -> d, idx -> idxv where
--   getIndexDocumentId :: idx -> DocumentId
--   getValue :: idx -> d -> idxv
--   getIndex :: idxv -> idx


-- class Monad m <= MonadDatabase schema m where
--   getUniqueDocument :: forall a. UniqueDocument schema a => m (Maybe a)
--   getDocument :: forall a. DocumentCollection a => m (Array a)
--   getFromIndex :: forall a b v. DocumentIndex a b v => b -> Maybe v -> Maybe v -> m (Array a)
--   putUniqueDocument :: forall a. UniqueDocument a => a -> m Unit
--   putDocument :: forall a. DocumentCollection a => a -> m Unit


-- testGetFromIndex :: forall a b v m. Monad m => DocumentCollection b => DocumentIndex a b v => b -> Maybe v -> Maybe v -> m (Array a)
-- testGetFromIndex idx min maxExcl = do
--   let indexId = getIndexDocumentId idx
--   ?todo

-- data LedgerSchema
--   = Ledger LedgerDocument
--   | AccountCollection AccountDocument

-- data TestIndex
--   = DateIndex
--   | CreatorIndex

-- data TestIndexValue
--   = DateIndexValue String
--   | CreatorIndexValue String

-- newtype TestDocument = TestDocument
--   { id :: String
--   , date :: String
--   , creator :: String
--   , nonIndexed:: String
--   }

-- instance documentCollectionTestDocument :: DocumentCollection TestDocument where
--   getDocumentId (TestDocument x) = ?todo

-- instance documentIndexTestDocument :: DocumentIndex TestDocument TestIndex TestIndexValue where
--   getIndexDocumentId DateIndex = IndexCollection <<< TransactionIndex <<< indexId $ "Date"
--   getIndexDocumentId CreatorIndex = IndexCollection <<< TransactionIndex <<< indexId $  "Creator"
--   getValue CreatorIndex (TestDocument x)= CreatorIndexValue <<<  _.creator $ x
--   getValue DateIndex (TestDocument x) = DateIndexValue <<< _.date $ x
--   getIndex (CreatorIndexValue _) = CreatorIndex
--   getIndex (DateIndexValue _) = DateIndex



-- -- class DocumentIndexRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
-- -- class DocumentIndexRecord rowlist row subrow | rowlist -> subrow where
-- --   appendRecord :: Proxy rowlist -> Record row -> Record row -> Record subrow

-- -- instance documentIndexRecordNil :: DocumentIndexRecord RL.Nil row () where
-- --   appendRecord _ _ _ = {}

-- -- instance documentIndexRecordCons ::
-- --   ( IsSymbol key
-- --   , Row.Cons key focus subrowTail subrow
-- --   , DocumentIndexRecord rowlistTail row subrowTail
-- --   , Semigroup focus
-- --   ) =>
-- --   DocumentIndexRecord (RL.Cons key focus rowlistTail) row subrow where
-- --   appendRecord _ ra rb = insert (get ra <> get rb) tail
-- --     where
-- --     key = reflectSymbol (Proxy :: Proxy key)
-- --     get = unsafeGet key :: Record row -> focus
-- --     insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
-- --     tail = appendRecord (Proxy :: Proxy rowlistTail) ra rb