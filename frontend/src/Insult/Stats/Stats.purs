module Insult.Stats where


import Prelude
import Halogen as H
import Halogen.HTML as HH
import Effect (Effect)
import Data.Maybe (Maybe (..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Insult.Insult (Insult)


foreign import f :: Array Insult -> Effect Unit

data Query a
  = Render  a
  | FetchInsults a

type Input = Unit

type State =
  { insults :: Array Insult
  }


component :: H.Component HH.HTML Query Input Void Aff
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState = { insults: [] }


render :: State -> H.ComponentHTML Query
render state =
  HH.div_ [ HH.text "o hai" ]
  
      
eval :: Query ~> H.ComponentDSL State Query Void Aff
eval = case _ of
  Render next -> do
    insults <- H.gets _.insults
    H.liftEffect $ do
      log "re-rendering stats"
      plotHistogram insults
    
    pure next

  FetchInsults next -> do
    H.liftEffect $ log "fetching"
    pure next
    
plotHistogram :: Array Insult -> Effect Unit
plotHistogram = f

