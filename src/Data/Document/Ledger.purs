module Data.Document.Ledger
  ( AccountDocument(..)
  , AccountDocumentRecord
  , LedgerDocument(..)
  , LedgerDocumentRecord
  , TransactionDocumentRecord
  , creditAccount
  , debitAccount
  , deleteTransaction
  , getAccount
  , getLedger
  , getTransaction
  , postTransaction
  , putAccount
  , putLedger
  , putTransaction
  )
  where

import Prelude

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract, toDocumentId)
import Capability.Storage (class DurableStorage, batchDeleteState, batchPutState, getState)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Common (AccountId, AccountType(..), Denomination, Instant, Money, TransactionId)
import Data.Contract.Document.Ledger (LedgerDocumentContract(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import Safe.Coerce (coerce)

type LedgerDocumentRecord =
  { name :: String
  , createdAt :: Instant
  }
newtype LedgerDocument = LedgerDocument LedgerDocumentRecord

instance encodeDataContractLedgerDocument :: EncodeDataContract LedgerDocumentContract LedgerDocument where
  toContract x = LedgerDocumentContractV1 <<< coerce $ x

instance decodeDataContractLedgerDocument :: DecodeDataContract LedgerDocumentContract LedgerDocument where
  fromContract (LedgerDocumentContractV1 d) = pure <<< coerce $ d

ledgerDocumentId :: String
ledgerDocumentId = "ledger"

getLedger :: ∀ m. (DurableStorage m) => MonadThrow Error m => m LedgerDocumentRecord
getLedger = do
  LedgerDocument x <- getState ledgerDocumentId
  pure x

putLedger :: ∀ m. (DurableStorage m) => LedgerDocumentRecord -> m Unit
putLedger x = batchPutState ledgerDocumentId <<< LedgerDocument $ x

type AccountDocumentRecord =
  { accountId :: AccountId
  , name :: String
  , accountType :: AccountType
  , denomination :: Denomination
  , balance :: Money
  , closed :: Boolean
  }
newtype AccountDocument = AccountDocument AccountDocumentRecord

derive instance genericAccountDocument :: Generic AccountDocument _
instance encodeJsonAccountDocument :: EncodeJson AccountDocument where
  encodeJson a = genericEncodeJson a

instance decodeJsonAccountDocument :: DecodeJson AccountDocument where
  decodeJson a = genericDecodeJson a

instance encodeDataContractAccountDocument :: EncodeDataContract AccountDocument AccountDocument where
  toContract = identity

instance decodeDataContractAccountDocument :: DecodeDataContract AccountDocument AccountDocument where
  fromContract = Just

getAccount :: ∀ m. (DurableStorage m) => MonadThrow Error m => AccountId -> m AccountDocumentRecord
getAccount id = do
  AccountDocument x <- getState <<< toDocumentId $ id
  pure x

putAccount :: ∀ m. (DurableStorage m) => AccountDocumentRecord -> m Unit
putAccount x = batchPutState (toDocumentId x.accountId) <<< AccountDocument $ x

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

derive instance genericTransactionDocument :: Generic TransactionDocument _
instance encodeJsonTransactionDocument :: EncodeJson TransactionDocument where
  encodeJson a = genericEncodeJson a

instance decodeJsonTransactionDocument :: DecodeJson TransactionDocument where
  decodeJson a = genericDecodeJson a

instance encodeDataContractTransactionDocument :: EncodeDataContract TransactionDocument TransactionDocument where
  toContract = identity

instance decodeDataContractTransactionDocument :: DecodeDataContract TransactionDocument TransactionDocument where
  fromContract = Just

getTransaction :: forall m. DurableStorage m => MonadThrow Error m => TransactionId -> m TransactionDocumentRecord
getTransaction id = do
  TransactionDocument x <- getState <<< toDocumentId $ id
  pure x

putTransaction :: forall m. (DurableStorage m) => TransactionDocumentRecord -> m Unit
putTransaction x = do
  -- TODO: update index
  batchPutState (toDocumentId x.transactionId) $ TransactionDocument x

postTransaction :: forall m. (DurableStorage m) => TransactionDocumentRecord -> m Unit
postTransaction x = do
  -- TODO: update index
  batchPutState (toDocumentId x.transactionId) $ TransactionDocument x

deleteTransaction :: forall m. (DurableStorage m) => TransactionId -> m Unit
deleteTransaction transactionId = do
  -- TODO: update index
  batchDeleteState <<< toDocumentId $ transactionId
