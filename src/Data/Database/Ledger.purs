module Data.Database.Ledger  where

import Prelude

import Capability.Storage.Database (class DatabaseDocument, class DatabaseId, class DatabaseIndex, class DocumentCollection, class DocumentId, class DatabaseDocumentId, class IndexedDocument, getIdPrefix)
import Capability.Utility (convertJsonErrorToError)
import Control.Alternative ((<|>))
import Data.Argonaut (decodeJson, encodeJson)
import Data.Common (AccountId, AccountType(..), Denomination, LedgerId, TransactionId, accountId, ledgerId, transactionId, unAccountId, unTransactionId)
import Data.Instant (Instant)
import Data.Map (singleton)
import Data.Maybe (Maybe(..))
import Data.Money (Money)
import Data.String (Pattern(..), stripPrefix)
import Safe.Coerce (coerce)
import Type.Prelude (Proxy(..))

data LedgerDatabaseId
  = DbLedgerId LedgerId
  | DbTransactionId TransactionId
  | DbAccountId AccountId

instance DatabaseId LedgerDatabaseId where
  dbIdString (DbLedgerId _) = "ledger"
  dbIdString (DbTransactionId x) = getIdPrefix (Proxy :: Proxy TransactionDocument) <> unTransactionId x
  dbIdString (DbAccountId x) = getIdPrefix (Proxy :: Proxy AccountDocument) <> unAccountId x
  dbIdFromString = case _ of
    "ledger" -> Just $ DbLedgerId ledgerId
    x -> DbAccountId <<< accountId <$> stripPrefix (Pattern $ getIdPrefix (Proxy :: Proxy AccountDocument)) x
      <|> DbTransactionId <<< transactionId <$> stripPrefix (Pattern $ getIdPrefix (Proxy :: Proxy TransactionDocument)) x

instance DatabaseDocumentId LedgerDatabaseId AccountId where
  wrapDocumentId = DbAccountId
  tryUnwrapDocumentId (DbAccountId x) = Just x
  tryUnwrapDocumentId _ = Nothing

instance DatabaseDocumentId LedgerDatabaseId TransactionId where
  wrapDocumentId = DbTransactionId
  tryUnwrapDocumentId (DbTransactionId x) = Just x
  tryUnwrapDocumentId _ = Nothing

instance DatabaseDocumentId LedgerDatabaseId LedgerId where
  wrapDocumentId = DbLedgerId
  tryUnwrapDocumentId (DbLedgerId x) = Just x
  tryUnwrapDocumentId _ = Nothing

data LedgerIndexes
  = TransactionSortKey

instance Eq LedgerIndexes where
  eq _ _ = true

instance Ord LedgerIndexes where
  compare _ _ = EQ

instance DatabaseIndex LedgerIndexes where
  getIndexId TransactionSortKey = "txn/sortKey"

type LedgerDocumentRecord =
  { name :: String
  , createdAt :: Instant
  }
newtype LedgerDocument = LedgerDocument LedgerDocumentRecord
unLedgerDocument :: LedgerDocument -> LedgerDocumentRecord
unLedgerDocument = coerce
ledgerDocument :: LedgerDocumentRecord -> LedgerDocument
ledgerDocument = coerce
instance DocumentId LedgerDocument LedgerId
instance DatabaseDocument LedgerDocument where
  decodeDocument json = convertJsonErrorToError <<< map LedgerDocument $ decodeJson json
  encodeDocument (LedgerDocument ledger) = encodeJson ledger

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
instance DocumentId AccountDocument AccountId
instance DatabaseDocument AccountDocument where
  decodeDocument json = convertJsonErrorToError <<< map AccountDocument $ decodeJson json
  encodeDocument (AccountDocument acct) = encodeJson acct
instance DocumentCollection AccountDocument where
  getIdPrefix _ = "acct/"

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
unTransactionDocument :: TransactionDocument -> TransactionDocumentRecord
unTransactionDocument = coerce
transactionDocument :: TransactionDocumentRecord -> TransactionDocument
transactionDocument = coerce
instance DocumentId TransactionDocument TransactionId
instance DatabaseDocument TransactionDocument where
  decodeDocument json = convertJsonErrorToError <<< map TransactionDocument $ decodeJson json
  encodeDocument (TransactionDocument tx) = encodeJson tx
instance IndexedDocument TransactionDocument LedgerIndexes where
  getRangeIndexes (TransactionDocument doc) = singleton TransactionSortKey doc.sortKey
instance DocumentCollection TransactionDocument where
  getIdPrefix _ = "txn/"