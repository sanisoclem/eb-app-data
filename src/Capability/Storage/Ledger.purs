module Capability.Storage.Ledger where

import Prelude

import Capability.Storage.Cf (class MonadCfStorage)
import Capability.Storage.Database (class MonadDatabase, class MonadIndexedDatabase, class MonadReadonlyDatabase, class MonadReadonlyIndexedDatabase, deleteIndexedDocument, getCollection, getDocument, getDocumentReadonly, putDocument, putIndexedDocument, tryGetDocument, tryGetDocumentReadonly)
import Capability.Storage.Transactional (class MonadTransactionalStorage)
import Capability.Utility (ensure)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Common (AccountId, TransactionId, balanceId, ledgerId)
import Data.Database.Ledger (AccountDocumentRecord, LedgerBalanceDocumentRecord, LedgerDatabaseId, LedgerDocumentRecord, LedgerIndexes, TransactionDocument, TransactionDocumentRecord, accountDocument, ledgerBalanceDocument, ledgerDocument, transactionDocument, unAccountDocument, unLedgerBalanceDocument, unLedgerDocument, unTransactionDocument)
import Data.Map (alter, empty)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Money (Money, zeroMoney)
import Effect.Exception (Error)
import Type.Prelude (Proxy(..))

class Monad m <= MonadLedgerReadonlyDb m where
  getLedgerReadonly :: m (Maybe LedgerDocumentRecord)
  getAccountReadonly :: AccountId -> m AccountDocumentRecord
  getAccountsReadonly :: m (Array AccountDocumentRecord)
  getBalancesReadonly :: m LedgerBalanceDocumentRecord
  getTransactionReadonly :: TransactionId -> m TransactionDocumentRecord

class Monad m <= MonadLedgerDb m where
  getLedger :: m (Maybe LedgerDocumentRecord)
  putLedger :: LedgerDocumentRecord -> m Unit
  getAccount :: AccountId -> m AccountDocumentRecord
  putAccount :: AccountDocumentRecord -> m Unit
  getBalances :: m LedgerBalanceDocumentRecord
  putBalances :: LedgerBalanceDocumentRecord -> m Unit
  getTransaction :: TransactionId -> m TransactionDocumentRecord
  putTransaction :: TransactionDocumentRecord -> m Unit
  postTransaction :: TransactionDocumentRecord -> m Unit
  deleteTransaction :: TransactionId -> m Unit

instance (Monad m, MonadThrow Error m, MonadDatabase LedgerDatabaseId m, MonadIndexedDatabase LedgerDatabaseId LedgerIndexes m, MonadTransactionalStorage m) => MonadLedgerDb m where
  getLedger = map unLedgerDocument <$> tryGetDocument ledgerId
  putLedger = putDocument ledgerId <<< ledgerDocument
  getAccount accountId = unAccountDocument <$> getDocument accountId
  putAccount x = putDocument x.accountId <<< accountDocument $ x
  getBalances = fromMaybe empty <$> map unLedgerBalanceDocument <$> tryGetDocument balanceId
  putBalances = putDocument balanceId <<< ledgerBalanceDocument
  getTransaction txId = unTransactionDocument <$> getDocument txId
  putTransaction x = putIndexedDocument x.transactionId <<< transactionDocument $ x
  postTransaction t = do
    (existing :: Maybe TransactionDocument) <- tryGetDocument t.transactionId
    ensure "no existing transaction with same id" $ isNothing existing
    putIndexedDocument t.transactionId $ transactionDocument t
  deleteTransaction txId = deleteIndexedDocument (Proxy :: Proxy TransactionDocument) txId

instance (Monad m, MonadThrow Error m, MonadReadonlyDatabase LedgerDatabaseId m, MonadReadonlyIndexedDatabase LedgerDatabaseId LedgerIndexes m, MonadCfStorage m) => MonadLedgerReadonlyDb m where
  getLedgerReadonly = map unLedgerDocument <$> tryGetDocumentReadonly ledgerId
  getAccountReadonly accountId = unAccountDocument <$> getDocumentReadonly accountId
  getTransactionReadonly txId = unTransactionDocument <$> getDocumentReadonly txId
  getBalancesReadonly = fromMaybe empty <$> map unLedgerBalanceDocument <$> tryGetDocumentReadonly balanceId
  getAccountsReadonly = map unAccountDocument <$> getCollection

type CreditDebitOperation = { accountId:: AccountId, amount :: Money }
mkCdo ∷ Money -> AccountId -> CreditDebitOperation
mkCdo amount accountId = { accountId, amount }

reverseBalances :: forall m. MonadLedgerDb m => Maybe CreditDebitOperation -> Maybe CreditDebitOperation -> m Unit
reverseBalances maybeCredit maybeDebit = updateBalances ((\c -> c { amount = (-c.amount) }) <$> maybeCredit) ((\c -> c { amount = (-c.amount) }) <$> maybeDebit)

updateBalances :: forall m. MonadLedgerDb m => Maybe CreditDebitOperation -> Maybe CreditDebitOperation -> m Unit
updateBalances maybeCredit maybeDebit = do
  bal <- updateBal credit maybeCredit <<< updateBal debit maybeDebit <<< { updated: false, balances: _ } <$> getBalances
  if bal.updated then putBalances bal.balances else pure unit
  where
    credit amount = case _ of
      Just x -> x { credits = x.credits + amount }
      Nothing -> { debits: zeroMoney, credits: amount }
    debit amount = case _ of
      Just x -> x { debits = x.debits + amount }
      Nothing -> { credits: zeroMoney, debits: amount }
    updateBal fn d bal = case d of
      Nothing -> bal
      Just x -> { updated: true, balances: alter (fn x.amount >>> Just) x.accountId bal.balances }
