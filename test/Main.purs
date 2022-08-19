module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Spec (SpecT, describe, pending)
import Test.Spec as Spec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Test.TestM (TestM, mkTestData, runTestM)

it :: ∀ m. Monad m => String -> TestM Unit -> SpecT Aff Unit m Unit
it desc = Spec.it desc <<< runTestM mkTestData

main :: Effect Unit
main = launchAff_ <<< void <<< identity <=< runSpecT defaultConfig [consoleReporter] $ testSpec

testSpec ∷ ∀ m. Monad m => SpecT Aff Unit m Unit
testSpec = do
  describe "Durable Object Handler" do
    describe "Request Handler" do
      pending "should validate request signature"
      pending "should not validate request signature if not yet bootstrapped"
      pending "should save outgoing messages to outbox"
      pending "should schedule alarm"
      pending "should commit batch"
      describe "Subscribe" do
        pending "should add subscription"
      describe "Unsubscribe" do
        pending "should remove subscription"
        pending "should be noop if subscription doesn't exist"
      describe "Bootstrap" do
        pending "should fail if already bootstrapped"
        pending "should create indexes"
        pending "should create outbox"
        pending "should return private key" -- sign requests with private key
    describe "Alarm Handler" do
      pending "should send outboxed messages"
      pending "should send max x times"
      pending "should not send more than maxAge"

  describe "Budget" do
    describe "Commands" do
      describe "CreateAccount" do
        pending "todo"
      describe "UpdateAccount" do
        pending "todo"
      describe "UpdateAllocations" do
        pending "should only allocate to liability and expense accounts"
        pending "should only allocate up to total asset amount"
  describe "Ledger" do
    describe "Commands" do
      describe "Bootstrap" do
        pending "should create ledger and balance document"

      describe "UpdateLedger" do
        pending "should update the ledger doc"
      describe "CreateAccount" do
        pending "should return the accountId"
        pending "should update balances doc"
        pending "should create account document"
      describe "UpdateAccount" do
        pending "should fail if account does not exist"
        pending "should update account document"
        pending "should update balance document"
      describe "CloseAccount" do
        pending "should fail if account has non zero balance"
        pending "should mark account as closed"
      describe "CreateTransaction" do
        pending "should create transaction document"
        pending "should update account balances if specified"
        pending "should fail if accounts are invalid"
        pending "should fail if credit and debit accounts are the same"
      describe "UpdateTransaction" do
        pending "should update transaction document"
        pending "should update account balances if specified"
        pending "should fail if accounts are invalid"
        pending "should fail if credit and debit accounts are the same"
      describe "DeleteTransaction" do
        pending "should delete transaction document"
        pending "should update account balances if specified"
