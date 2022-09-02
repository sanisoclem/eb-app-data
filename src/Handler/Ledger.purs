module Handlers.Ledger
  ( handleCommand
  , handleQuery
  ) where

import Prelude

import Capability.Now (class MonadNow, nowUtc)
import Capability.RandomId (generateId)
import Capability.Storage.Ledger (class MonadLedgerDb, class MonadLedgerReadonlyDb, deleteTransaction, getAccount, getAccountsReadonly, getBalancesReadonly, getLedger, getLedgerReadonly, getTransaction, mkCdo, postTransaction, putAccount, putLedger, putTransaction, reverseBalances, updateBalances, getTransactionsReadonly)
import Capability.Storage.Outbox (class MonadOutbox, queue)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Command.Ledger (LedgerCommand(..))
import Data.Event.Ledger (LedgerEvent(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Query.Ledger (LedgerQuery(..), LedgerQueryResult(..))
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)

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
      -- ensure "Account balance should be zero" $ account.balance == zeroMoney
      putAccount account { closed = true }
      queue AccountClosed
    CreateTransactionV1 x -> do
      transactionId <- generateId
      postTransaction
        { transactionId
        , date: x.date
        , credit: x.credit
        , debit: x.debit
        , amount: x.amount
        , notes: x.notes
        }
      updateBalances (mkCdo x.amount <$> x.credit) (mkCdo x.amount <$> x.debit)
      queue TransactionCreated
      queue BalanceUpdated
    UpdateTransactionV1 x -> do
      prevTrans <- getTransaction x.transactionId
      -- reverse prev transaction
      reverseBalances (mkCdo prevTrans.amount <$> prevTrans.credit) (mkCdo prevTrans.amount <$> prevTrans.debit)

      let updatedTrans = prevTrans { amount = x.amount
        , notes = x.notes
        , date = x.date
        , debit = x.debit
        , credit = x.credit
      }

      putTransaction updatedTrans
      updateBalances (mkCdo updatedTrans.amount <$> updatedTrans.credit) (mkCdo updatedTrans.amount <$> updatedTrans.debit)
      queue TransactionUpdated
      queue BalanceUpdated
    DeleteTransactionV1 transactionId -> do
      trans <- getTransaction transactionId
      reverseBalances (mkCdo trans.amount <$> trans.credit) (mkCdo trans.amount <$> trans.debit)
      deleteTransaction transactionId
      queue TransactionDeleted
      queue BalanceUpdated

handleQuery
  :: ∀ m
   . MonadLedgerReadonlyDb m
  => MonadThrow Error m
  => LedgerQuery
  -> m LedgerQueryResult
handleQuery = case _ of
  GetLedgerV1 -> do
    ledger <- getLedgerReadonly
    accounts <- getAccountsReadonly
    pure $
      GetLedgerResultV1
        { name: fromMaybe "" (ledger <#> _.name)
        , accounts: accounts
        }
  GetBalancesV1 -> do
    GetBalancesResultV1 <$> getBalancesReadonly
  GetTransactionsV1 x -> do
    GetTransactionsResultV1 <$> getTransactionsReadonly x.from x.to