module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Console (log)
import Effect.Ref as Ref
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.PushState (makeInterface)
import FRP.Event (create, subscribe)

import Component.App (Query(..))
import App (AppQuery (..), runAppM)
import Component.App as App

main :: Effect Unit
main = HA.runHalogenAff do
  body  <- HA.awaitBody
  event <- H.liftEffect create
  state <- H.liftEffect $ Ref.new 
        { session: Nothing
        }

  let env = 
        { state: state
        , push: event.push
        }
  
  let component = H.hoist (flip runAppM env) App.component
  driver <- runUI component unit body
  nav    <- H.liftEffect makeInterface  
  _      <- H.liftEffect $ subscribe event.event (handler driver)
  H.liftEffect $ nav.pushState (unsafeToForeign {}) "#insult"

handler :: H.HalogenIO Query Void Aff -> AppQuery -> Effect Unit
handler driver pt = do
  case pt of
    AppNavigate route -> do
      log $ show route 
      _ <- launchAff $ driver.query <<< H.action <<< NavigateTo $ route
      pure unit
