module Main where

import Prelude

import App as App
import Effect (Effect)
import Effect.Aff (forkAff)
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.PushState (makeInterface)

main :: Effect Unit
main = HA.runHalogenAff do
  body   <- HA.awaitBody
  driver <- runUI App.component unit body
  nav    <- H.liftEffect makeInterface  
  _      <- forkAff $ App.routeSignal driver
  H.liftEffect $ nav.pushState (unsafeToForeign {}) "#insult/from"
