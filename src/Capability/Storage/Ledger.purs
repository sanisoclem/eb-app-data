module Capability.Storage.Ledger where

import Prelude

import Capability.Storage.Cf (class MonadCfStorage)
import Capability.Storage.Database (class MonadDatabase, class MonadIndexedDatabase, class MonadReadonlyDatabase, class MonadReadonlyIndexedDatabase, deleteIndexedDocument, getDocument, getDocumentReadonly, putDocument, putIndexedDocument, tryGetDocument, tryGetDocumentReadonly)
import Capability.Storage.Transactional (class MonadTransactionalStorage)
import Capability.Utility (ensure)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Common (AccountId, TransactionId, ledgerId)
import Data.Database.Ledger (AccountDocumentRecord, LedgerDatabaseId, LedgerDocumentRecord, LedgerIndexes, TransactionDocument, TransactionDocumentRecord, accountDocument, ledgerDocument, transactionDocument, unAccountDocument, unLedgerDocument, unTransactionDocument)
import Data.Maybe (Maybe, isJust)
import Effect.Exception (Error)
import Type.Prelude (Proxy(..))

class Monad m <= MonadLedgerReadonlyDb m where
  getLedgerReadonly :: m (Maybe LedgerDocumentRecord)
  getAccountReadonly :: AccountId -> m AccountDocumentRecord
  getTransactionReadonly :: TransactionId -> m TransactionDocumentRecord

class Monad m <= MonadLedgerDb m where
  getLedger :: m (Maybe LedgerDocumentRecord)
  putLedger :: LedgerDocumentRecord -> m Unit
  getAccount :: AccountId -> m AccountDocumentRecord
  putAccount :: AccountDocumentRecord -> m Unit
  getTransaction :: TransactionId -> m TransactionDocumentRecord
  putTransaction :: TransactionDocumentRecord -> m Unit
  postTransaction :: TransactionDocumentRecord -> m Unit
  deleteTransaction :: TransactionId -> m Unit

instance (Monad m, MonadThrow Error m, MonadDatabase LedgerDatabaseId m, MonadIndexedDatabase LedgerDatabaseId LedgerIndexes m, MonadTransactionalStorage m) => MonadLedgerDb m where
  getLedger = map unLedgerDocument <$> tryGetDocument ledgerId
  putLedger = putDocument ledgerId <<< ledgerDocument
  getAccount accountId = unAccountDocument <$> getDocument accountId
  putAccount x = putDocument x.accountId <<< accountDocument $ x
  getTransaction txId = unTransactionDocument <$> getDocument txId
  putTransaction x = putIndexedDocument x.transactionId <<< transactionDocument $ x
  postTransaction t = do
    (existing :: Maybe TransactionDocument) <- tryGetDocument t.transactionId
    ensure "no existing transaction with same id" $ isJust existing
    putIndexedDocument t.transactionId $ transactionDocument t
  deleteTransaction txId = deleteIndexedDocument (Proxy :: Proxy TransactionDocument) txId


instance (Monad m, MonadThrow Error m, MonadReadonlyDatabase LedgerDatabaseId m, MonadReadonlyIndexedDatabase LedgerDatabaseId LedgerIndexes m, MonadCfStorage m) => MonadLedgerReadonlyDb m where
  getLedgerReadonly = map unLedgerDocument <$> tryGetDocumentReadonly ledgerId
  getAccountReadonly accountId = unAccountDocument <$> getDocumentReadonly accountId
  getTransactionReadonly txId = unTransactionDocument <$> getDocumentReadonly txId
