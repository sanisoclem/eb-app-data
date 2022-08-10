module Data.Document.Ledger
  ( LedgerDocument(..)
  , getLedger
  , putLedger
  )
  where

import Prelude

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract)
import Capability.Storage (class DurableStorage, getDoState, putDoState)
import Data.Common (Instant)
import Data.Contract.Document.Ledger (LedgerDocumentContract(..))
import Safe.Coerce (coerce)

newtype LedgerDocument = LedgerDocument
  { name :: String
  , createdAt :: Instant
  }

instance encodeDataContractLedgerDocument :: EncodeDataContract LedgerDocumentContract LedgerDocument where
  toContract x = LedgerDocumentContractV1 <<< coerce $ x
instance decodeDataContractLedgerDocument :: DecodeDataContract LedgerDocumentContract LedgerDocument where
  fromContract (LedgerDocumentContractV1 d) = pure <<< coerce $ d

ledgerDocumentId :: String
ledgerDocumentId = "ledger"

getLedger :: ∀ m. (DurableStorage m) => m _
getLedger = do
  LedgerDocument x <- getDoState "ledger"
  pure x

putLedger :: ∀ m. (DurableStorage m) => _ -> m Unit
putLedger x = putDoState ledgerDocumentId <<< LedgerDocument $ x
