module Krank.Control.MonadState
  ( class MonadState
  , getState
  , modifyState
  , setState
  ) where

import Halogen (HalogenM, lift)
import Prelude (class Monad, Unit, const, (<<<))

-- | Naïve state monad representation. For simplicity, we use two operations:
-- | one for reading and one for writing.
class Monad m <= MonadState s m | m -> s where
  getState    :: m s
  modifyState :: (s -> s) -> m Unit

-- | We define a helper function for setting the state when we don't
-- | care about the current state.
setState :: ∀ s m. MonadState s m => s -> m Unit
setState = modifyState <<< const

-- | We need a HalogenM instance in order to be able to use this DSL
-- | within our component's `eval`.
instance stateDSLHalogenM :: MonadState s' m => MonadState s' (HalogenM s f g p o m) where
  getState    = lift getState
  modifyState = lift <<< modifyState
