module Capability.Storage.Ledger where

import Prelude
import Data.Document.Ledger (AccountDocumentRecord, LedgerDocumentRecord, TransactionDocumentRecord)
import Data.Common (AccountId, TransactionId)
import Control.Monad.Trans.Class (class MonadTrans, lift)

class Monad m <= MonadLedgerStorage m where
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