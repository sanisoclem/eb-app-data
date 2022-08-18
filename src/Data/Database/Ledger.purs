module Data.Database.Ledger  where

import Prelude

import Capability.Storage.Database (class DatabaseDocument, class DatabaseId, class DocumentId)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Common (AccountId, AccountType(..), Denomination, LedgerId, TransactionId, ledgerId, unAccountId, unTransactionId)
import Data.Instant (Instant)
import Data.Maybe (Maybe)
import Data.Money (Money)
import Safe.Coerce (coerce)

data LedgerDatabaseId
  = DbLedgerId LedgerId
  | DbTransactionId TransactionId
  | DbAccountId AccountId

instance DatabaseId LedgerDatabaseId where
  dbIdString (DbLedgerId _) = "ledger"
  dbIdString (DbTransactionId x) = "txn/" <> unTransactionId x
  dbIdString (DbAccountId x) = "acct/" <> unAccountId x

instance DocumentId LedgerDatabaseId AccountId where
  wrapDocumentId = DbAccountId

instance DocumentId LedgerDatabaseId TransactionId where
  wrapDocumentId = DbTransactionId

instance DocumentId LedgerDatabaseId LedgerId where
  wrapDocumentId = DbLedgerId

type LedgerDocumentRecord =
  { name :: String
  , createdAt :: Instant
  }
newtype LedgerDocument = LedgerDocument LedgerDocumentRecord
unLedgerDocument :: LedgerDocument -> LedgerDocumentRecord
unLedgerDocument = coerce
ledgerDocument :: LedgerDocumentRecord -> LedgerDocument
ledgerDocument = coerce
instance DatabaseDocument LedgerDocument LedgerId where
  getDocumentId _ = ledgerId
  decode json = LedgerDocument <$> decodeJson json
  encode (LedgerDocument ledger) = encodeJson ledger

type AccountDocumentRecord =
  { accountId :: AccountId
  , name :: String
  , accountType :: AccountType
  , denomination :: Denomination
  , balance :: Money
  , closed :: Boolean
  }
newtype AccountDocument = AccountDocument AccountDocumentRecord
unAccountDocument :: AccountDocument -> AccountDocumentRecord
unAccountDocument = coerce
accountDocument :: AccountDocumentRecord -> AccountDocument
accountDocument = coerce
instance DatabaseDocument AccountDocument AccountId where
  getDocumentId (AccountDocument x) = x.accountId
  decode json = AccountDocument <$> decodeJson json
  encode (AccountDocument acct) = encodeJson acct

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
instance DatabaseDocument TransactionDocument TransactionId where
  getDocumentId (TransactionDocument x) = x.transactionId
  decode json = TransactionDocument <$> decodeJson json
  encode (TransactionDocument tx) = encodeJson tx
