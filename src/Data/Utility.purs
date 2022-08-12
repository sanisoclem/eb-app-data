module Data.Utility where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Effect.Exception (Error, error)

ensure :: ∀ m. MonadThrow Error m => String -> Boolean -> m Unit
ensure err = case _ of
  true -> throwError $ error err
  _ -> pure unit
