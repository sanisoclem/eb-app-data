module Test.Main where

import Prelude

import Capability.DataContract (encodeContractJson, generateId, toDocumentId)
import Data.Common (AccountType(..), Denomination(..), mkInstant, mkMoney)
import Data.Document.Ledger (AccountDocument(..), LedgerDocument(..), getAccount, getLedger)
import Data.Identity (Identity)
import Data.Interface.Ledger (LedgerCommand(..), LedgerRequest(..))
import Data.Map (empty, singleton)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Handlers.Ledger (handleLedgerRequest)
import Test.Spec (SpecT, describe, pending, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec, runSpecT)
import Test.TestM (TestData, TestM, mkTestData, runTestM)


main = identity <=< runTestM mkTestData <<< runSpecT defaultConfig [consoleReporter] $ do
  describe "Ledger Handler" do
    describe "Commands" do
      describe "UpdateLedger" do
        let name = "test"
        -- let request = LedgerCommand $ UpdateLedger { name }
        -- let docs = singleton "ledger" <<< encodeContractJson $ LedgerDocument { name: "old", createdAt: mkInstant 0 }
        -- let d = mkTestData request docs
        it "should update the ledger name" do
          resp <- handleLedgerRequest
          updated <- getLedger
          updated.name `shouldEqual` name
          resp.statusCode `shouldEqual` 200

      describe "CreateAccount" do
        -- let request = LedgerCommand $ CreateAccount { name: "testAccount"
        --   , accountType:  Income
        --   , denomination: Currency "AUD"
        --   }
        -- let d = mkTestData request empty
        it "should return the accountId" do
          resp <- handleLedgerRequest
          resp.statusCode `shouldEqual` 200
        it "should create a new account" do
          resp <- handleLedgerRequest
          resp.statusCode `shouldEqual` 200
      describe "UpdateAccount" do
        accountId <- generateId
        let name = "test"
        -- let request = LedgerCommand $ UpdateAccount { accountId, name }
        -- let docs = singleton (toDocumentId accountId) <<< encodeContractJson $ AccountDocument { accountId
        --   , name: "old"
        --   , accountType: Income
        --   , denomination: Currency "AUD"
        --   , balance: mkMoney 0
        --   , closed: false
        --   }
        -- let d = mkTestData request empty
        it "should update account name" do
          resp <- handleLedgerRequest
          updated <- getAccount accountId
          updated.name `shouldEqual` name
          resp.statusCode `shouldEqual` 200

      pending "transaction index must be updated"