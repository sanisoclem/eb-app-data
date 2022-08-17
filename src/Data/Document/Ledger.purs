module Data.Document.Ledger  where

import Prelude

import Data.Common (AccountId, AccountType(..), Denomination, TransactionId)
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe)
import Data.Money (Money)

type LedgerDocumentRecord =
  { name :: String
  , createdAt :: Instant
  }
newtype LedgerDocument = LedgerDocument LedgerDocumentRecord

type AccountDocumentRecord =
  { accountId :: AccountId
  , name :: String
  , accountType :: AccountType
  , denomination :: Denomination
  , balance :: Money
  , closed :: Boolean
  }
newtype AccountDocument = AccountDocument AccountDocumentRecord

debitAccount :: Money -> AccountDocumentRecord -> AccountDocumentRecord
debitAccount amount account = case account.accountType of
  Income -> account { balance = account.balance - amount }
  Expense -> account { balance = account.balance + amount }
  Liability -> account { balance = account.balance - amount }
  Asset -> account { balance = account.balance + amount }

creditAccount :: Money -> AccountDocumentRecord -> AccountDocumentRecord
creditAccount amount = debitAccount (-amount)

type TransactionDocumentRecord =
  { transactionId :: TransactionId
  , sortKey :: Int
  , credit :: Maybe AccountId
  , debit :: Maybe AccountId
  , amount :: Money
  , notes :: String
  }

newtype TransactionDocument = TransactionDocument TransactionDocumentRecord
