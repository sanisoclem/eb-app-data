module Ledger where

import Prelude

import AppM (AppM, runAppM)
import Capability.DurableObject (class DurableObject, getBodyJson, getRequestMethod, notFoundResponse, stringResponse)
import Context (mkContext)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Ledger (LedgerEvent(..))
import Data.Request (RequestMethod(..))
import Effect.Aff (Aff)
import FFI.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState, doErrorResponse)

fetchMain :: DurableObjectState -> DurableObjectRequest -> Aff DurableObjectResponse
fetchMain state req = either identity identity <$> lmap doErrorResponse <$> runAppM (mkContext state req) handler

handler :: AppM DurableObjectResponse
handler = do
  method <- getRequestMethod
  case method of
    POST -> getBodyJson >>= handlerPost
    GET -> handlerGet
    _ -> notFoundResponse "Not found"


handlerPost :: forall m. (DurableObject m) => LedgerEvent -> m DurableObjectResponse
handlerPost (UpdateLedger x) = stringResponse "OK"
handlerPost (CreateAccount x) = stringResponse "OK"

handlerGet :: AppM DurableObjectResponse
handlerGet = do
  stringResponse "GetING"