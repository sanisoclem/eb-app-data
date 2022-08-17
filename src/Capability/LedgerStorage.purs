module Capability.LedgerStorage where

import Prelude
import Data.Document.Ledger (AccountDocumentRecord, LedgerDocumentRecord, TransactionDocumentRecord)
import Data.Common (AccountId, TransactionId)

class Monad m <= LedgerStorage m where
  getLedger :: m LedgerDocumentRecord
  putLedger :: LedgerDocumentRecord -> m Unit
  getAccount :: AccountId -> m AccountDocumentRecord
  putAccount :: AccountDocumentRecord -> m Unit
  getTransaction :: TransactionId -> m TransactionDocumentRecord
  putTransaction :: TransactionDocumentRecord -> m Unit
  postTransaction :: TransactionDocumentRecord -> m Unit
  deleteTransaction :: TransactionId -> m Unit
