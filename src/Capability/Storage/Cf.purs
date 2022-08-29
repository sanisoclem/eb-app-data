module Capability.Storage.Cf where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Argonaut (Json)
import Data.Either (note)
import Data.Maybe (Maybe)
import Effect.Exception (Error, error)

class Monad m <= MonadCfStorage m where
  tryGetState :: String -> m (Maybe Json)
  putState :: String -> Json -> m Unit
  deleteState :: String -> m Unit
  getStateByPrefix :: String -> m (Array Json)

class Monad m <= MonadCfStorageBatch m where
  runBatch :: { puts:: Array { docId :: String, body :: Json }, deletes :: Array String } -> m Unit


getState :: forall m. MonadThrow Error m => MonadCfStorage m => String -> m Json
getState = liftEither <<< note (error "state not found") <=< tryGetState
