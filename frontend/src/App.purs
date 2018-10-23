module App
  ( AppState (..)
  , AppQuery (..)
  , AppM
  , runAppM
  ) where


import Krank.Control.MonadNavigate (class MonadNavigate)
import Krank.Control.MonadState (class MonadState)
import Prelude

import Common.Session (Session)
import Control.Monad.Reader (ask, asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Router (Route)
import Type.Equality as TE

type AppEnv = 
  { state :: Ref AppState
  , push  :: AppQuery -> Effect Unit
  }

type AppState =
  { session :: Maybe Session
  }
  
-- Uses a ReaderT (Ref AppState) instead of StateT AppState to avoid issues with
-- https://github.com/slamdata/purescript-halogen/issues/386
newtype AppM a = AppM (ReaderT AppEnv Aff a)
derive instance newtypeAppM :: Newtype (AppM a) _
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

runAppM :: forall a. AppM a -> AppEnv -> Aff a
runAppM app state = do
  runReaderT (unwrap app) state

-- | We need a HalogenM instance in order to be able to use this DSL
-- | within our component's `eval`.
--instance monadStateHalogenM :: Reader AppState m => Reader AppState (HalogenM s f g p o m) where
--  state = H.lift state

{-instance monadStateAppM :: MonadState AppState AppM where
  state f = do
    ref <- AppM ask
    s   <- H.liftEffect $ Ref.read ref
    let Tuple a s' = f s
    H.liftEffect $ Ref.write s' ref
    pure a    
-}

instance monadAskAppM :: TE.TypeEquals e AppEnv => MonadAsk e AppM where
  ask = AppM $ asks TE.from

-- | Encode get/set state. We use the same trick as for `MonadAsk`, using
-- | our environment's `state`.
instance monadStateAppM :: TE.TypeEquals s AppState => MonadState s AppM where
  getState = AppM do
    env <- ask
    H.liftEffect $ TE.from <$> Ref.read env.state

  modifyState f = AppM do
    env <- ask
    H.liftEffect $ Ref.modify_ (TE.to <<< f <<< TE.from) env.state

data AppQuery
  = AppNavigate Route

-- | Navigate will simply use the `push` part of our environment
-- | to send the new route to the router through our event listener.
instance monadNavigateAppM :: MonadNavigate AppM where
  navigate route = AppM do
    env <- ask
    H.liftEffect $ env.push $ AppNavigate route
