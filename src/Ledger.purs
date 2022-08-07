module Ledger where

import Prelude

import AppM (runAppM)
import Capability.DurableObject (class DurableObject, errorResponse, getBodyJson, getRequestMethod, notFoundResponse, stringResponse)
import Context (mkContext)
import Control.Monad.Error.Class (class MonadError, catchError)
import Control.Promise (Promise, fromAff)
import Data.Ledger (LedgerEvent(..))
import Data.Request (RequestMethod(..))
import Effect (Effect)
import Effect.Exception (Error)
import FFI.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState)

fetchMain :: DurableObjectState -> DurableObjectRequest -> Effect (Promise DurableObjectResponse)
fetchMain state req = fromAff $ runAppM (mkContext state req) handler

handler :: forall m. (DurableObject m) => (MonadError Error m) => m DurableObjectResponse
handler = catchError go errorResponse
  where
    go = do
      method <- getRequestMethod
      case method of
        POST -> getBodyJson >>= handlerPost
        GET -> handlerGet
        _ -> notFoundResponse "Not found"

handlerPost :: forall m. (DurableObject m) => LedgerEvent -> m DurableObjectResponse
handlerPost (UpdateLedger x) = stringResponse "OK"
handlerPost (CreateAccount x) = stringResponse "OK"

handlerGet :: forall m. (DurableObject m) => m DurableObjectResponse
handlerGet = do
  stringResponse "GetING"