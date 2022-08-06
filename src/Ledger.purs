module Ledger where

import Prelude

import AppM (AppM, runAppM)
import Data.Request (RequestMethod (..))
import Capability.DurableObject (getRequestMethod, stringResponse)
import Context (mkContext)
import Effect.Aff (Aff)
import FFI.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState)

fetchMain :: DurableObjectState -> DurableObjectRequest -> Aff DurableObjectResponse
fetchMain state req = runAppM context handler
  where
    context = mkContext state req

handler :: AppM DurableObjectResponse
handler = do
  method <- getRequestMethod
  case method of
    POST -> handlerPost
    GET -> handlerGet
    PUT -> handlerPut
    _ -> stringResponse "Not found"


handlerPost :: AppM DurableObjectResponse
handlerPost = do
  stringResponse "POSTING"

handlerGet :: AppM DurableObjectResponse
handlerGet = do
  stringResponse "GetING"

handlerPut :: AppM DurableObjectResponse
handlerPut = do
  stringResponse "PutING"