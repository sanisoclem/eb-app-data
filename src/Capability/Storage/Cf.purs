module EB.DB.Capability.Storage.Cf where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Argonaut (Json)
import Data.Either (note)
import Data.Maybe (Maybe)
import Effect.Exception (Error, error)

class Monad m <= MonadCfStorage m where
  tryGetDurableState :: String -> m (Maybe Json)
  putDurableState :: String -> Json -> m Unit
  deleteDurableState :: String -> m Unit
  getDurableStateByPrefix :: String -> m (Array Json)

class Monad m <= MonadCfStorageBatch m where
  runBatch :: { puts:: Array { docId :: String, body :: Json }, deletes :: Array String } -> m Unit

getDurableState :: forall m. MonadThrow Error m => MonadCfStorage m => String -> m Json
getDurableState = liftEither <<< note (error "state not found") <=< tryGetDurableState
