module Capability.Storage.Database where

import Prelude

import Capability.Codec (class Decodable, decode)
import Capability.Storage.Transactional (class MonadTransactionalStorage, batchGetState)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Rec.Class (tailRecM)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit, unit)
import Data.Void (Void, absurd)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Proxy (Proxy(..))

class DocumentId id schema | id -> schema where
  documentIdString :: id -> String

-- getDocument :: forall m a docId schema. MonadThrow Error m => MonadTransactionalStorage m => Decodable a => DocumentId docId => docId -> m a
-- getDocument docId = do
--   liftEither <=< map decode <<< batchGetState <<< documentIdString $ docId

class DocumentIndex idx idxv schema | idx -> schema, idx -> idxv where
  getIndexId :: idx -> String

class (Monad m, DocumentId docId schema) <= MonadDatabase schema docId idx idxv m | schema -> docId, schema -> idx, idx -> idxv where
  getDocument :: forall a. Decodable a => DocumentId docId schema => docId -> m a
  getFromIndex :: forall a. DocumentIndex idx idxv schema => idx -> Maybe idxv -> Maybe idxv -> m (Array a)
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