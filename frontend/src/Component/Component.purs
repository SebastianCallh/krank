module Component.Component where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
{-
renderWithHeader :: forall a. String -> H.ComponentHTML a -> H.ComponentHTML a
renderWithHeader txt component =
  HH.div_
  [ header txt
  , component
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
          , HH.li_ [ menuLink SelectSender "Nytt kränk" "fa fa-bullhorn" ]
          , HH.li_ [ menuLink Stats "Statistik" "fa fa-bar-chart"]
          , HH.li_ [ menuLink Admin "Profil" "fa fa-user"]
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

    ulClass = ""

    menuLink route text iconClass =
      HH.a [ HP.class_ $ activeIfRoute route
           , HP.href $ routeFor route
           ]
        [ HH.div_
          [ HH.i [ HP.class_ $ H.ClassName iconClass ] []
          , HH.text text
          ]
        ]

      case state.navStatus of
        Extended  -> "extended"
        Collapsed -> "collapsed"
-}
