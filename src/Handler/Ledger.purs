module Handlers.Ledger
  ( handleCommand
  , handleQuery
  ) where

import Prelude

import Capability.Storage.Ledger (class MonadLedgerDb, deleteTransaction, getAccount, getLedger, getTransaction, postTransaction, putAccount, putLedger, putTransaction)
import Capability.RandomId (generateId)
import Capability.Storage.Outbox (class MonadOutbox, queue)
import Capability.Utility (ensure)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Command.Ledger (LedgerCommand(..))
import Data.Common (AccountId)
import Data.Database.Ledger (creditAccount, debitAccount)
import Data.Event.Ledger (LedgerEvent(..))
import Data.Maybe (Maybe)
import Data.Money (Money, zeroMoney)
import Data.Query.Ledger (LedgerQuery(..))
import Data.Traversable (sequence)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)

handleCommand
  :: ∀ m
   . MonadLedgerDb m
  => MonadOutbox LedgerEvent m
  => MonadThrow Error m
  => MonadEffect m
  => LedgerCommand
  -> m Unit
handleCommand = case _ of
    UpdateLedger x -> do
      putLedger =<< _ { name = x.name } <$> getLedger
      queue LedgerUpdated
    CreateAccount x -> do
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
    UpdateAccount x -> do
      account <- getAccount x.accountId
      putAccount account { name = x.name }
      queue AccountUpdated
    CloseAccount accountId -> do
      account <- getAccount accountId
      ensure "Account balance should be zero" $ account.balance == zeroMoney
      putAccount account { closed = true }
      queue AccountClosed
    CreateTransaction x -> do
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
    UpdateTransaction x -> do
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
    DeleteTransaction transactionId -> do
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
   . MonadLedgerDb m
  => MonadThrow Error m
  => LedgerQuery
  -> m Unit -- TODO
handleQuery = case _ of
  GetLedger -> do
    ledger <- getLedger
    pure unit
  _ -> pure unit