module Capability.Storage.Ledger where

import Prelude

import Capability.Storage.Cf (class MonadCfStorage)
import Capability.Storage.Database (class MonadDatabase, class MonadIndexedDatabase, class MonadReadonlyDatabase, class MonadReadonlyIndexedDatabase, deleteIndexedDocument, getCollection, getDocument, getDocumentReadonly, getFromRangeIndexReadonly, putDocument, putIndexedDocument, tryGetDocument, tryGetDocumentReadonly)
import Capability.Storage.Transactional (class MonadTransactionalStorage)
import Capability.Utility (ensure)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (filter)
import Data.Common (AccountId, TransactionId, balanceId, ledgerId, unAccountId)
import Data.Database.Ledger (AccountDocument, AccountDocumentRecord, LedgerBalanceDocumentRecord, LedgerDatabaseId, LedgerDocumentRecord, LedgerIndexes(..), TransactionDocument, TransactionDocumentRecord, accountDocument, emptyBalance, ledgerBalanceDocument, ledgerDocument, transactionDocument, unAccountDocument, unLedgerBalanceDocument, unLedgerDocument, unTransactionDocument)
import Data.Instant (Instant, unInstant)
import Data.Map (alter)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Money (Money, zeroMoney)
import Effect.Exception (Error)
import Type.Prelude (Proxy(..))

class Monad m <= MonadLedgerReadonlyDb m where
  getLedgerReadonly :: m (Maybe LedgerDocumentRecord)
  getAccountReadonly :: AccountId -> m AccountDocumentRecord
  getAccountsReadonly :: m (Array AccountDocumentRecord)
  getBalancesReadonly :: m LedgerBalanceDocumentRecord
  getTransactionReadonly :: TransactionId -> m TransactionDocumentRecord
  getTransactionsReadonly :: Maybe Instant -> Maybe Instant -> m (Array TransactionDocumentRecord)

class Monad m <= MonadLedgerDb m where
  getLedger :: m (Maybe LedgerDocumentRecord)
  putLedger :: LedgerDocumentRecord -> m Unit
  getAccount :: AccountId -> m AccountDocumentRecord
  ensureAccountExists :: AccountId -> m Unit
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
  ensureAccountExists accountId= do
    (maybeAccount :: Maybe AccountDocument) <- tryGetDocument accountId
    ensure ("Account does not exist " <> unAccountId accountId) $ isJust maybeAccount

  putAccount x = putDocument x.accountId <<< accountDocument $ x
  getBalances = fromMaybe emptyBalance <$> map unLedgerBalanceDocument <$> tryGetDocument balanceId
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
  getBalancesReadonly = fromMaybe emptyBalance <$> map unLedgerBalanceDocument <$> tryGetDocumentReadonly balanceId
  getAccountsReadonly = filter (not <<< _.closed) <<< map unAccountDocument <$> getCollection
  getTransactionsReadonly f t = do
    let from = unInstant <$> f
    let to = unInstant <$> t
    map unTransactionDocument <$> getFromRangeIndexReadonly TransactionSortKey from to

type CreditDebitOperation = { accountId:: AccountId, amount :: Money }
mkCdo ∷ Money -> AccountId -> CreditDebitOperation
mkCdo amount accountId = { accountId, amount }

updateBalances :: forall m. MonadLedgerDb m => Money -> Maybe AccountId -> Maybe AccountId -> m Unit
updateBalances amount creditAct debitAct = do
  updatedBalance <- debit amount debitAct <<< credit amount creditAct <$> getBalances
  putBalances updatedBalance
  where
    credit amt ca bal = case ca of
      Just act -> bal { accountBalances = alter (Just <<< fromMaybe { debits: zeroMoney, credits: amt } <<< map (\b -> b { credits = b.credits + amt })) act bal.accountBalances }
      Nothing -> bal { floatingBalance = bal.floatingBalance { credits = bal.floatingBalance.credits + amt } }
    debit amt da bal = case da of
      Just act -> bal { accountBalances = alter (Just <<< fromMaybe { credits: zeroMoney, debits: amt } <<< map (\b -> b { debits = b.debits + amt })) act bal.accountBalances }
      Nothing -> bal { floatingBalance = bal.floatingBalance { debits = bal.floatingBalance.debits + amt } }

