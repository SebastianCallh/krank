module Insult.Stats where


import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Insult.Api as Api
import Insult.Insult (Insult)

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


component :: H.Component HH.HTML Query Input Void Aff
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
      NotFetched -> HH.div [ HP.id_ "histogram" ] [ ]
      Fetching   -> HH.div_  [ HH.text "Laddar..." ]
      Fetched insults ->
        HH.div_
        [ HH.canvas [ HP.id_ "histogram" ]
        ]

      Failed err ->
        HH.div_
        [ HH.text err
        ]
  ]

  
eval :: Query ~> H.ComponentDSL State Query Void Aff
eval = case _ of
  FetchInsults next -> do
    result <- H.liftEffect $ launchAff fetchInsults
    pure next

  where
    fetchInsults :: Aff Unit
    fetchInsults = do
      einsults <- Api.fetchInsults
      case einsults of
        Left err -> do
          H.liftEffect $ log err
          pure unit
          
        Right insults -> do
          H.liftEffect $ plotHistogram $ show insults              
          pure unit
          

