module Capability.Storage.Ledger where

import Prelude

import Capability.Storage.Database (class MonadDatabase, class MonadIndexedDatabase, deleteIndexedDocument, getDocument, putDocument, putIndexedDocument, tryGetDocument)
import Capability.Storage.Transactional (class MonadTransactionalStorage)
import Capability.Utility (ensure)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Common (AccountId, TransactionId, ledgerId)
import Data.Database.Ledger (AccountDocumentRecord, LedgerDatabaseId, LedgerDocumentRecord, LedgerIndexes, TransactionDocument, TransactionDocumentRecord, accountDocument, ledgerDocument, transactionDocument, unAccountDocument, unLedgerDocument, unTransactionDocument)
import Data.Maybe (Maybe, isJust)
import Effect.Exception (Error)
import Type.Prelude (Proxy(..))

class Monad m <= MonadLedgerDb m where
  getLedger :: m LedgerDocumentRecord
  putLedger :: LedgerDocumentRecord -> m Unit
  getAccount :: AccountId -> m AccountDocumentRecord
  putAccount :: AccountDocumentRecord -> m Unit
  getTransaction :: TransactionId -> m TransactionDocumentRecord
  putTransaction :: TransactionDocumentRecord -> m Unit
  postTransaction :: TransactionDocumentRecord -> m Unit
  deleteTransaction :: TransactionId -> m Unit

instance (Monad m, MonadThrow Error m, MonadDatabase LedgerDatabaseId m, MonadIndexedDatabase LedgerDatabaseId LedgerIndexes m, MonadTransactionalStorage m) => MonadLedgerDb m where
  getLedger = unLedgerDocument <$> getDocument ledgerId
  putLedger = putDocument <<< ledgerDocument
  getAccount accountId = unAccountDocument <$> getDocument accountId
  putAccount = putDocument <<< accountDocument
  getTransaction txId = unTransactionDocument <$> getDocument txId
  putTransaction = putIndexedDocument <<< transactionDocument
  postTransaction t = do
    (existing :: Maybe TransactionDocument) <- tryGetDocument t.transactionId
    ensure "no existing transaction with same id" $ isJust existing
    putIndexedDocument $ transactionDocument t
  deleteTransaction txId = deleteIndexedDocument (Proxy :: Proxy TransactionDocument) txId
