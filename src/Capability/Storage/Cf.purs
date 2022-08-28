module Capability.Storage.Cf where

import Prelude

import Data.Argonaut (Json)
import Data.Maybe (Maybe)

class Monad m <= MonadCfStorage m where
  tryGetState :: String -> m (Maybe Json)
  putState :: String -> Json -> m Unit
  deleteState :: String -> m Unit

class Monad m <= MonadCfStorageBatch m where
  runBatch :: { puts:: Array { docId :: String, body :: Json }, deletes :: Array String } -> m Unit