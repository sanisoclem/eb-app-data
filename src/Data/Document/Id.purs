module Data.Document.Id where

outboxDocumentId :: String
outboxDocumentId = "outbox"

ledgerDocumentId :: String
ledgerDocumentId = "ledger"

accountIdPrefix :: String
accountIdPrefix = "account/"

transactionIdPrefix :: String
transactionIdPrefix = "txn/"