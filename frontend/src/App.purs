module App where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Console (log)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Insult.Creator as Insult
import Insult.Stats as Stats
import Router (Route(..), routeFor, routes)
import Routing.Hash (matches)
import User as User

type Input = Unit


data Query a
  = NavigateTo Route a


type State =
  { currentRoute :: Route
  }
  

type ChildQuery = Coproduct3 Insult.Query Stats.Query Insult.Query
type ChildSlot  = Either3 Unit Unit Unit


component :: H.Component HH.HTML Query Input Void Aff
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState = { currentRoute: SelectSender }
    

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
    render state =
        case state.currentRoute of
          SelectSender->
              HH.div_
                [ header "Vem kränkte?"
                , HH.slot' contentSlot unit Insult.component Nothing absurd
                ]
                    
          SelectReceiver userName ->
            case User.fromUserName userName of
              Left _err ->
                HH.div_
                [ header "404"
                , HH.h2_ [ HH.text $ "Ingen användare vid namn " <> userName]
                ]
                
              Right user ->
                HH.div_
                [ header $ (User.userName user) <> " kränkte vem?"
                , HH.slot' contentSlot unit Insult.component (Just user) absurd
                ]

          Stats ->
            HH.div_
              [ header "Statistik"
              , HH.slot' statsSlot unit Stats.component unit absurd
              ]


    header text =
      HH.div [ HP.class_ $ H.ClassName "header" ]
      [ HH.h1_ [ HH.text text ]
      , HH.a
        [ HP.class_ $ H.ClassName "top-right"
        , HP.href $ routeFor Stats
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fa fa-bar-chart" ] []
        ]
      , HH.hr_
      ]
      
    contentSlot = CP.cp1
    statsSlot   = CP.cp2


    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
    eval = case _ of
      NavigateTo route next -> do
        H.modify_ _ { currentRoute = route }        
        pure next
      

routeSignal :: H.HalogenIO Query Void Aff -> Aff (Effect Unit)
routeSignal driver = do
  H.liftEffect (matches routes hashChanged)
  where
    hashChanged _ newRoute = do
      H.liftEffect $ log $ "updated " <> show newRoute
      _ <- launchAff $ driver.query <<< H.action $ NavigateTo newRoute
      pure unit
