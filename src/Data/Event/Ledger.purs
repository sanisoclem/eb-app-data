module Data.Event.Ledger where

data LedgerEvent
  = LedgerUpdated
  | AccountCreated
  | AccountUpdated
  | AccountClosed
  | BalanceUpdated
  | TransactionCreated
  | TransactionUpdated
  | TransactionDeleted