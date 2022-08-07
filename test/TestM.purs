module Test.TestM where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Safe.Coerce (coerce)

newtype TestM a = TestM (Aff a)

runTestM :: ∀ a. TestM a -> Aff a
runTestM = coerce

derive newtype instance functorTestM :: Functor TestM
derive newtype instance applyTestM :: Apply TestM
derive newtype instance applicativeTestM :: Applicative TestM
derive newtype instance bindTestM :: Bind TestM
derive newtype instance monadTestM :: Monad TestM
derive newtype instance monadEffectTestM :: MonadEffect TestM
derive newtype instance monadAffTestM :: MonadAff TestM
-- TODO: create DurableObject instance