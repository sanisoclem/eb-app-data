module Handlers.DurableObject where

import Prelude

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract, fromContract, toContract)
import Capability.IncomingRequest (class IncomingRequest, getBodyJson)
import Capability.Storage (class DurableStorage, commitBatchState)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Array (fromFoldable)
import Data.Common (SubscriptionId)
import Data.Document.DurableObject (addToOutbox, clearOutbox, getOutbox)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Response (Response, errorResponse, jsonResponse)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error)

handleFetch
  :: ∀ m cmd qry evt cmdContract qryContract f evtContract
   . DurableStorage m
  => MonadEffect m
  => MonadError Error m
  => IncomingRequest m
  => DecodeDataContract cmdContract cmd
  => DecodeDataContract qryContract qry
  => EncodeDataContract evtContract evt
  => Foldable f
  => Functor f
  => (∀ m'
     . DurableStorage m'
    => MonadError Error m'
    => MonadEffect m'
    => cmd
    -> m' (f evt)
    )
  -> (∀ m'
     . DurableStorage m'
    => MonadError Error m'
    => MonadEffect m'
    => qry
    -> m' Response
    )
  -> m Response
handleFetch handleCmd handleQry = catchError go (pure <<< errorResponse)
  where
    go = getBodyJson >>= case _ of
      Bootstrap -> pure $ errorResponse (error "Not implemented")
      Subscribe _  -> pure $ errorResponse (error "Not implemented")
      Unsubscribe _ -> pure $ errorResponse (error "Not implemented")
      Command cmd -> do
        evts <- handleCmd cmd
        addToOutbox evts
        commitBatchState
        pure <<< jsonResponse 200 <<< fromFoldable $ toContract <$> evts
      Query qry -> handleQry qry

handleAlarm
  :: ∀ m
   . DurableStorage m
  => MonadThrow Error m
  => m Unit
handleAlarm = do
  _outbox <- getOutbox
  -- TODO: send evts
  clearOutbox

data DurableObjectRequest cmd qry
  = Subscribe String -- TODO: use proper uri type
  | Unsubscribe SubscriptionId
  | Bootstrap
  | Command cmd
  | Query qry

data DurableObjectRequestContract cmd qry
  = SubscribeV1 String
  | UnsubscribeV1 SubscriptionId
  | BootstrapV1
  | CommandV1 cmd
  | QueryV1 qry

instance decodeJsonDurableObjectRequest :: (DecodeJson cmd, DecodeJson qry) => DecodeJson (DurableObjectRequestContract cmd qry) where
  decodeJson json = do
    obj <- decodeJson json
    obj .: "tag" >>= case _ of
      "boot/v1" -> pure BootstrapV1
      "sub/v1" -> SubscribeV1 <$> obj .: "callbackUrl"
      "unsub/v1" -> UnsubscribeV1 <$> obj .: "subscriptionId"
      "cmd/v1" -> CommandV1 <$> obj .: "body"
      "qry/v1" -> QueryV1 <$> obj .: "body"
      _ -> throwError $ UnexpectedValue json

instance decodeDataContractDurableObjectRequest :: (DecodeDataContract cmd' cmd, DecodeDataContract qry' qry) =>  DecodeDataContract (DurableObjectRequestContract cmd' qry') (DurableObjectRequest cmd qry) where
  fromContract (SubscribeV1 x) = Just $ Subscribe x
  fromContract (UnsubscribeV1 x) = Just $ Unsubscribe x
  fromContract (CommandV1 x) = Command <$> fromContract x
  fromContract (QueryV1 x) = Query <$> fromContract x
  fromContract BootstrapV1 = Just Bootstrap