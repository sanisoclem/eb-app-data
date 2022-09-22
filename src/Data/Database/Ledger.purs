module EB.DB.Data.Database.Ledger  where

import Prelude

import Control.Alternative ((<|>))
import Data.Argonaut (decodeJson, encodeJson)
import Data.Map (Map, empty, singleton)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripPrefix)
import EB.DB.Capability.Storage.Database (class DatabaseDocument, class DatabaseDocumentId, class DatabaseId, class DatabaseIndex, class DocumentCollection, class DocumentId, class IndexedDocument, getIdPrefix)
import EB.DB.Data.Utility (convertJsonErrorToError)
import EB.DB.Data.Common (AccountId, AccountType, BalanceId, Denomination, LedgerId, TransactionId, accountId, balanceId, ledgerId, transactionId, unAccountId, unTransactionId)
import EB.DB.Data.Instant (Instant, unInstant)
import EB.DB.Data.Money (Money, zeroMoney)
import Safe.Coerce (coerce)
import Type.Prelude (Proxy(..))

data LedgerDatabaseId
  = DbLedgerId LedgerId
  | DbBalanceId BalanceId
  | DbTransactionId TransactionId
  | DbAccountId AccountId

instance DatabaseId LedgerDatabaseId where
  dbIdString (DbLedgerId _) = "ledger"
  dbIdString (DbBalanceId _) = "balance"
  dbIdString (DbTransactionId x) = getIdPrefix (Proxy :: Proxy TransactionDocument) <> unTransactionId x
  dbIdString (DbAccountId x) = getIdPrefix (Proxy :: Proxy AccountDocument) <> unAccountId x
  dbIdFromString = case _ of
    "ledger" -> Just $ DbLedgerId ledgerId
    "balance" -> Just $ DbBalanceId balanceId
    x -> DbAccountId <<< accountId <$> stripPrefix (Pattern $ getIdPrefix (Proxy :: Proxy AccountDocument)) x
      <|> DbTransactionId <<< transactionId <$> stripPrefix (Pattern $ getIdPrefix (Proxy :: Proxy TransactionDocument)) x

instance DatabaseDocumentId LedgerDatabaseId AccountId where
  wrapDocumentId = DbAccountId
  tryUnwrapDocumentId (DbAccountId x) = Just x
  tryUnwrapDocumentId _ = Nothing

instance DatabaseDocumentId LedgerDatabaseId BalanceId where
  wrapDocumentId = DbBalanceId
  tryUnwrapDocumentId (DbBalanceId x) = Just x
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

type LedgerBalanceDocumentRecord =
  { accountBalances :: Map AccountId { debits :: Money, credits :: Money }
  , floatingBalance :: { debits :: Money, credits :: Money }
  }
newtype LedgerBalanceDocument = LedgerBalanceDocument LedgerBalanceDocumentRecord
emptyBalance :: LedgerBalanceDocumentRecord
emptyBalance = { accountBalances: empty, floatingBalance: { debits: zeroMoney, credits: zeroMoney }}
unLedgerBalanceDocument :: LedgerBalanceDocument -> LedgerBalanceDocumentRecord
unLedgerBalanceDocument = coerce
ledgerBalanceDocument :: LedgerBalanceDocumentRecord -> LedgerBalanceDocument
ledgerBalanceDocument = coerce
instance DocumentId LedgerBalanceDocument BalanceId
instance DatabaseDocument LedgerBalanceDocument where
  decodeDocument json = convertJsonErrorToError <<< map LedgerBalanceDocument $ decodeJson json
  encodeDocument (LedgerBalanceDocument ledger) = encodeJson ledger

type AccountDocumentRecord =
  { accountId :: AccountId
  , name :: String
  , accountType :: AccountType
  , denomination :: Denomination
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

type TransactionDocumentRecord =
  { transactionId :: TransactionId
  , date :: Instant
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
  getRangeIndexes (TransactionDocument doc) = singleton TransactionSortKey $ unInstant doc.date
instance DocumentCollection TransactionDocument where
  getIdPrefix _ = "txn/"