module App where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Insult.Admin as Admin
import Insult.Creator as Creator
import Insult.Stats as Stats
import Router (Route(..), routeFor, routes)
import Routing.Hash (matches)
import User as User

type Input = Unit

data Query a
  = NavigateTo Route a
  | ToggleNav a

data NavStatus = Extended | Collapsed

flipNavStatus :: NavStatus -> NavStatus
flipNavStatus Extended = Collapsed
flipNavStatus Collapsed = Extended

type State =
  { currentRoute :: Route
  , navStatus    :: NavStatus
  }
  
type ChildQuery = Coproduct3 Creator.Query Stats.Query Admin.Query
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
    initialState =
      { currentRoute: SelectSender
      , navStatus: Collapsed
      }


render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
render state =
  case state.currentRoute of
    SelectSender->
      HH.div_
      [ header "Vem kränkte?"
      , content $ HH.slot' contentSlot unit Creator.component Nothing absurd
      ]
                    
    SelectReceiver userName ->
      case User.fromUserName userName of
        Left _err ->
          HH.div_
          [ header "404"
          , content $ HH.h2_ [ HH.text $ "Ingen användare vid namn " <> userName]
          ]
                
        Right user ->
          HH.div_
          [ header $ (User.userName user) <> " kränkte vem?"
          , content $ HH.slot' contentSlot unit Creator.component (Just user) absurd
          ]

    Stats ->
      HH.div_
      [ header "Statistik"
      , content $ HH.slot' statsSlot unit Stats.component unit absurd
      ]

    Admin ->
      HH.div_
      [ header "Admin"
      , content $ HH.slot' adminSlot unit Admin.component unit absurd
      ]
      
  where
    header text =
      HH.div [ HP.class_ $ H.ClassName "header" ]
      [ HH.h1_ [ HH.text text ]
      , HH.nav [ HP.class_ $ H.ClassName ulClass ]
        [ HH.ul_
          [ HH.li_ [ HH.a
                     [ HP.href "#"
                     , HP.class_ $ H.ClassName "menu-text"
                     ] [ HH.text "Meny" ]
                   ]
          , HH.li_ [ menuLink SelectSender "active" "Nytt kränk" "fa fa-bullhorn" ]
          , HH.li_ [ menuLink Stats "inactive" "Statistik" "fa fa-bar-chart"]
          ]
        ]
      , HH.div [ HP.id_ "menuToggle" ]
          [ HH.input
            [ HP.type_ $ HP.InputCheckbox
            , HE.onChange $ HE.input_ ToggleNav
            ]
          , HH.div [HP.id_ "spanwrap" ]
            [ HH.span_ []
            , HH.span_ []
            , HH.span_ []
            ]
          ]
      ]

      
    ulClass =
      case state.navStatus of
        Extended  -> "extended"
        Collapsed -> "collapsed"

    content body =
      HH.div [ HP.class_ $ H.ClassName "content" ] [ body ]
      
    headerClassName =
      case state.navStatus of
        Extended  -> "topnav responsive"
        Collapsed -> "topnav"
        
    menuLink route activeClass text iconClass =
      HH.a [ HP.class_ $ activeIfRoute route
           , HP.href $ routeFor route
           ]
        [ HH.div_
          [ HH.i [ HP.class_ $ H.ClassName iconClass ] []
          , HH.text text
          ]
        ]

    activeIfRoute route =
      H.ClassName $
      if state.currentRoute == route
      then "active"
      else ""

    contentSlot = CP.cp1
    statsSlot   = CP.cp2
    adminSlot   = CP.cp3

eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
eval (NavigateTo route next) = do
  H.modify_ _ { currentRoute = route
              , navStatus    = Collapsed
              }
  pure next

eval (ToggleNav next) = do
  H.modify_ $ \s -> s { navStatus = flipNavStatus s.navStatus }
  pure next
      
routeSignal :: H.HalogenIO Query Void Aff -> Aff (Effect Unit)
routeSignal driver =
  H.liftEffect (matches routes hashChanged)
  where
    hashChanged _ newRoute = do
      _ <- launchAff $ driver.query <<< H.action $ NavigateTo newRoute
      pure unit
