module Test.Main where

import Prelude

import Capability.DataContract (encodeContractJson)
import Data.Argonaut (encodeJson, stringify)
import Data.Argonaut.Core as A
import Data.Common (mkInstant)
import Data.Document.Ledger (LedgerDocument(..))
import Data.Interface.Ledger (LedgerCommand(..), LedgerRequest(..))
import Data.Map (singleton)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign.Object (fromFoldable)
import Handlers.Ledger (handleLedgerRequest)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.TestM (mkTestData, runTestM)


main :: Effect Unit
main = launchAff_ <<< runSpec [consoleReporter] $ do
  describe "Ledger Handler" do
    describe "Commands" do
      describe "UpdateLedger" do
        let request = LedgerCommand $ UpdateLedger { name: "test" }
        let docs = singleton "ledger" <<< encodeContractJson $ LedgerDocument { name: "old", createdAt: mkInstant 0 }
        let d = mkTestData (stringify <<< encodeContractJson $ request) docs
        it "should update the ledger name" <<< runTestM d $ do
          resp <- handleLedgerRequest
          stringify resp.body `shouldEqual` "{\"message\":\"OK\"}"
          resp.statusCode `shouldEqual` 200
      pending "transaction index must be updated"