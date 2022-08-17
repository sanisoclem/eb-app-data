module Data.Document.Id
  ( class DocumentId
  , toDocumentId
  , fromDocumentId
  )
  where

import Prelude
import Safe.Coerce (coerce)
import Data.Common (AccountId(..), TransactionId(..))
import Data.Maybe (Maybe)
import Data.String (Pattern(..), stripPrefix)

class DocumentId i where
  toDocumentId :: i -> String
  fromDocumentId :: String -> Maybe i

accountIdPrefix :: String
accountIdPrefix = "account/"

transactionIdPrefix :: String
transactionIdPrefix = "txn/"

instance documentIdAccountId :: DocumentId AccountId where
  fromDocumentId = coerce <<< stripPrefix (Pattern  accountIdPrefix)
  toDocumentId = (<>) accountIdPrefix <<< coerce

instance documentIdTransactionId :: DocumentId TransactionId where
  fromDocumentId = coerce <<< stripPrefix (Pattern transactionIdPrefix)
  toDocumentId = (<>) transactionIdPrefix <<< coerce