module EB.DB.Capability.Utility where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut (JsonDecodeError, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Exception (Error, error)

ensure :: ∀ m. MonadThrow Error m => String -> Boolean -> m Unit
ensure err = case _ of
  false -> throwError $ error err
  _ -> pure unit

convertJsonErrorToError :: forall a. Either JsonDecodeError a -> Either Error a
convertJsonErrorToError = lmap (error <<< printJsonDecodeError)