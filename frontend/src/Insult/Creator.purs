module Insult.Creator where

import Data.Time
import Prelude

import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.String.Common (toLower)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, error, killFiber, launchAff)
import Effect.Console (log)
import Effect.Now (nowTime)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Insult.Api (saveInsult)
import Insult.Insult (Insult(..), Amount(..))
import Router (Route(..), routeFor)
import User (User, userName, allUsers)
import Web.UIEvent.MouseEvent (MouseEvent)

foreign import playSound :: Unit -> Effect Unit

data Query a
  = IncreaseInsult User User a
  | ShowMessage String a
  | HandleInput Input a
  
type Input = Maybe User

type State = 
  { msender  :: Maybe User
  , mmessage :: Maybe String
  , statuses :: Map (Tuple User User) InsultStatus 
  }

type InsultStatus =
  { fiber   :: Fiber Int
  , amount  :: Amount
  , expires :: Maybe Time
  }

updateStatusFiber :: Time -> Fiber Int -> Maybe InsultStatus-> Maybe InsultStatus
updateStatusFiber expires' fiber' mstatus =
  pure $ case mstatus of
    Nothing -> { fiber: fiber', amount: one, expires: pure expires' }
    Just status -> status { fiber = fiber', amount = status.amount + Amount 1}

isStatusExpired :: Time -> InsultStatus -> Boolean
isStatusExpired t status = maybe true (t >_) status.expires 

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
      , statuses: M.empty
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
        onClick receiver = HE.input_ $ IncreaseInsult sender receiver
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
eval (IncreaseInsult from to next) = do
    let key = Tuple from to
    H.liftEffect $ playSound unit    
    mstatus <- H.gets _.statuses
       >>= M.lookup key
       >>> pure

    let delayTime = Milliseconds 2000.0
    fiber' <- H.liftEffect $ case mstatus of
      Nothing -> do
        newFiber one delayTime
        
      Just status -> do
        t <- nowTime
        if isStatusExpired t status
          then newFiber one delayTime
          else do
            _ <- launchAff $ killFiber (error "increased") status.fiber
            newFiber (status.amount + Amount 1) delayTime
    
    (Tuple _ expTime) <- H.liftEffect $ nowTime
      >>= adjust delayTime
      >>> pure
      
    H.modify_ $ \s -> s { statuses = M.alter (updateStatusFiber expTime fiber') key s.statuses }
    
    pure next
  
    where
      newFiber :: Amount -> Milliseconds  -> Effect (Fiber Int)
      newFiber amount ms = do
        launchAff $ do
          delay ms
          saveInsult $ Insult
            { from: from
            , to: to
            , amount: amount
            }

      
eval (ShowMessage msg next) = do
    H.modify_ _ { mmessage = Just msg }
    pure next

eval (HandleInput muser next) = do
    H.modify_ _ { msender = muser }
    pure next
