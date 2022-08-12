module AppM where

import Prelude

import Context (ContextData)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.State (class MonadState, StateT, runStateT)
import Data.Tuple (fst)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Safe.Coerce (coerce)

newtype AppM a = AppM (StateT ContextData Aff a)

runAppM :: ∀ a. ContextData -> AppM a -> Aff a
runAppM state = map fst <<< (runStateT <@> state) <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadThrowAppM :: MonadThrow Error AppM
derive newtype instance monadErrorAppM :: MonadError Error AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadState ContextData AppM