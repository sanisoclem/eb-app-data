module Capability.Storage.Ledger where

import Prelude

import Capability.Storage.Database (class MonadDatabase, getDocument, putDocument)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Common (AccountId, TransactionId, ledgerId)
import Data.Database.Ledger (AccountDocumentRecord, LedgerDatabaseId, LedgerDocument, LedgerDocumentRecord, TransactionDocumentRecord, accountDocument, ledgerDocument, unAccountDocument, unLedgerDocument)
import Effect.Exception (Error)

class Monad m <= MonadLedgerDb m where
  getLedger :: m LedgerDocumentRecord
  putLedger :: LedgerDocumentRecord -> m Unit
  getAccount :: AccountId -> m AccountDocumentRecord
  putAccount :: AccountDocumentRecord -> m Unit
  getTransaction :: TransactionId -> m TransactionDocumentRecord
  putTransaction :: TransactionDocumentRecord -> m Unit
  postTransaction :: TransactionDocumentRecord -> m Unit
  deleteTransaction :: TransactionId -> m Unit

-- instance ledgerStorageMonadTrans :: (Monad (t m), LedgerStorage m, MonadTrans t) => LedgerStorage (t m) where
--   getLedger = lift getLedger
--   putLedger = lift <<< putLedger
--   getAccount = lift <<< getAccount
--   putAccount = lift <<< putAccount
--   getTransaction = lift <<< getTransaction
--   putTransaction = lift <<< putTransaction
--   postTransaction = lift <<< postTransaction
--   deleteTransaction = lift <<< deleteTransaction

instance (Monad m, MonadDatabase LedgerDatabaseId m) => MonadLedgerDb m where
  getLedger = unLedgerDocument <$> getDocument ledgerId
  putLedger = putDocument <<< ledgerDocument
  getAccount accountId = unAccountDocument <$> getDocument accountId
  putAccount = putDocument <<< accountDocument
  getTransaction = ?todo
  putTransaction = ?todo
  postTransaction = ?todo
  deleteTransaction = ?todo

getLedger2 :: forall m. MonadThrow Error m => MonadDatabase LedgerDatabaseId m => m LedgerDocumentRecord
getLedger2 = unLedgerDocument <$> getDocument ledgerId