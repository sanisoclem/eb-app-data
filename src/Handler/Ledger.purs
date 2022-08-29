module Handlers.Ledger
  ( handleCommand
  , handleQuery
  ) where

import Prelude

import Capability.Now (class MonadNow, nowUtc)
import Capability.RandomId (generateId)
import Capability.Storage.Ledger (class MonadLedgerDb, class MonadLedgerReadonlyDb, deleteTransaction, getAccount, getLedger, getLedgerReadonly, getTransaction, postTransaction, putAccount, putLedger, putTransaction)
import Capability.Storage.Outbox (class MonadOutbox, queue)
import Capability.Utility (ensure)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Command.Ledger (LedgerCommand(..))
import Data.Common (AccountId)
import Data.Database.Ledger (creditAccount, debitAccount)
import Data.Event.Ledger (LedgerEvent(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Money (Money, zeroMoney)
import Data.Query.Ledger (LedgerQuery(..), LedgerQueryResult(..))
import Data.Traversable (sequence)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error)

handleCommand
  :: ∀ m
   . MonadLedgerDb m
  => MonadOutbox LedgerEvent m
  => MonadThrow Error m
  => MonadEffect m
  => MonadNow m
  => LedgerCommand
  -> m Unit
handleCommand = case _ of
    UpdateLedgerV1 x -> do
      ledger <- getLedger
      createdAt <- nowUtc
      putLedger $
        case ledger of
          Just l -> l { name = x.name }
          _ -> { name: x.name, createdAt }
      queue LedgerUpdated
    CreateAccountV1 x -> do
      accountId <- generateId
      putAccount
        { accountId
        , accountType: x.accountType
        , balance: zeroMoney
        , denomination: x.denomination
        , name: x.name
        , closed: false
        }
      queue AccountCreated
    UpdateAccountV1 x -> do
      account <- getAccount x.accountId
      putAccount account { name = x.name }
      queue AccountUpdated
    CloseAccountV1 accountId -> do
      account <- getAccount accountId
      ensure "Account balance should be zero" $ account.balance == zeroMoney
      putAccount account { closed = true }
      queue AccountClosed
    CreateTransactionV1 x -> do
      transactionId <- generateId
      postTransaction
        { transactionId
        , sortKey: x.sortKey
        , credit: x.credit
        , debit: x.debit
        , amount: x.amount
        , notes: x.notes
        }
      getDebitPut x.debit x.amount
      getCreditPut x.credit x.amount
      queue TransactionCreated
      queue BalanceUpdated
    UpdateTransactionV1 x -> do
      prevTrans <- getTransaction x.transactionId
      -- reverse prev transaction
      getDebitPut prevTrans.credit prevTrans.amount
      getCreditPut prevTrans.debit prevTrans.amount

      let updatedTrans = prevTrans { amount = x.amount
        , notes = x.notes
        , sortKey = x.sortKey
        , debit = x.debit
        , credit = x.credit
      }

      putTransaction updatedTrans
      getDebitPut updatedTrans.debit updatedTrans.amount
      getCreditPut updatedTrans.credit updatedTrans.amount
      queue TransactionUpdated
      queue BalanceUpdated
    DeleteTransactionV1 transactionId -> do
      trans <- getTransaction transactionId
      getDebitPut trans.credit trans.amount
      getCreditPut trans.debit trans.amount
      deleteTransaction transactionId
      queue TransactionDeleted
      queue BalanceUpdated

getDebitPut :: ∀ m. MonadThrow Error m => MonadLedgerDb m => Maybe AccountId -> Money -> m Unit
getDebitPut maybeAccount amount = void <<< sequence $ (putAccount <<< debitAccount amount <=< getAccount) <$> maybeAccount

getCreditPut :: ∀ m. MonadThrow Error m => MonadLedgerDb m => Maybe AccountId -> Money -> m Unit
getCreditPut maybeAccount amount = void <<< sequence $ (putAccount <<< creditAccount amount <=< getAccount) <$> maybeAccount

handleQuery
  :: ∀ m
   . MonadLedgerReadonlyDb m
  => MonadThrow Error m
  => LedgerQuery
  -> m LedgerQueryResult
handleQuery = case _ of
  GetLedgerV1 -> do
    ledger <- getLedgerReadonly
    pure $
      GetLedgerResultV1
        { name: fromMaybe "" (ledger <#> _.name)
        , accounts: []
        }
  _ -> do
    throwError $ error "not implemented"