module Component.Admin where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import User (User)

data Query a
  = One a
  
type Input = Unit -- Maybe User

type State = Maybe Boolean
--  { thing :: Maybe User
--  }

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: const Nothing --initialState
    , render
    , eval
    , receiver: const Nothing
    }
--  where
{-    initialState muser =
      { thing: Nothing
      }-}

render :: State -> H.ComponentHTML Query
render state =  HH.div []
  [ HH.text "admin"
  ]

eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval (One next) = pure next
