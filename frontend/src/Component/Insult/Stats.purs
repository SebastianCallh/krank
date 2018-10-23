module Component.Insult.Stats where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Insult.Insult (Insult)
import Api as Api
import App (AppM)

foreign import plotHistogram :: String -> Effect Unit

data Query a
  = FetchInsults a

type Error = String

data InsultStatus
  = NotFetched
  | Fetching
  | Fetched (Array Insult)
  | Failed Error

type Input = Unit

type State =
  { insults :: InsultStatus
  }

component :: H.Component HH.HTML Query Input Void AppM
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: pure $ H.action FetchInsults
    , finalizer: Nothing
    }
  where
    initialState =
      { insults: NotFetched
      }

render :: State -> H.ComponentHTML Query
render state =
  HH.div
  [ HP.class_ $ H.ClassName "stats"
  ]
  [ case state.insults of
      NotFetched      -> HH.div_ [ HH.text "Ingen statistik hämtad" ]
      Fetching        -> HH.div [ HP.class_ $ H.ClassName "loader" ] []
      Fetched insults -> HH.div [ HP.id_ "histogram" ] [ ]
      Failed err      -> HH.div_ [ HH.text err ]
  ]
  
eval :: Query ~> H.ComponentDSL State Query Void AppM
eval = case _ of
  FetchInsults next -> do
    H.modify_ _ { insults = Fetching }
    H.liftAff Api.fetchInsults >>= case _ of
      Left err -> 
        H.liftEffect $ log err
           
      Right insults -> do
        H.modify_ _ { insults = Fetched insults }
        H.liftEffect $ plotHistogram $ show insults              
        
    pure next
