module Insult.Creator where

import Prelude


import Data.Maybe (Maybe(..))
import Data.String.Common (toLower)
import Effect.Aff (Aff, launchAff)
import Effect (Effect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Insult.Insult (Insult(..))
import Insult.Api (saveInsult)
import Router (Route(..), routeFor)
import User (User, userName, allUsers)
import Web.UIEvent.MouseEvent (MouseEvent)

foreign import playSound :: Unit -> Effect Unit

data Query a
  = SendInsult User User a
  | ShowMessage String a
  | HandleInput Input a
  
type Input = Maybe User

type State = 
  { msender  :: Maybe User
  , mmessage :: Maybe String
  }
    

component :: H.Component HH.HTML Query Input Void Aff
component =
  H.component
    { initialState: initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where
    initialState muser =
      { msender:  muser
      , mmessage: Nothing
      }


render :: State -> H.ComponentHTML Query
render state = 
  case state.msender of
    Nothing     -> selectFromList
    Just sender -> selectToList sender
  
  where
    selectFromList =
      let
        userHref user = routeFor $ SelectReceiver $ userName user
        onClick = const $ pure Nothing 
      in
        userPanel userHref onClick

          
    selectToList sender =
      let
        userHref = const $ routeFor $ SelectReceiver $ userName sender
        onClick receiver = HE.input_ $ SendInsult sender receiver
      in
      userPanel userHref onClick


userPanel
  :: forall t1 t2. (User -> String)
  -> (User -> MouseEvent -> Maybe t1)
  -> HH.HTML t2 t1
userPanel userHref userClicked =
  HH.div [ HP.class_ $ HH.ClassName "user-panel"] userButtons
  where
    userButtons =
      map userButton allUsers
                  
    userButton user =
      HH.a
      [ HP.class_ $ buttonClass user
      , HP.href $ userHref user
      , HE.onClick $ userClicked user
      ]
      [ HH.text $ userName user ]

    buttonClass user =
      H.ClassName $ "user-button " <> toLower (userName user)


eval :: Query ~> H.ComponentDSL State Query Void Aff
eval = case _ of
  SendInsult from to next -> do
    let insult = Insult
          { from: from
          , to: to
          , amount: 1
          }

    _ <- H.liftEffect do
      playSound unit    
      launchAff $ saveInsult $
         Insult { from: from
                , to: to
                , amount: 1
                }
    pure next
     
  ShowMessage msg next -> do
    H.modify_ _ { mmessage = Just msg }
    pure next

  HandleInput muser next -> do
    H.modify_ _ { msender = muser }
    pure next
