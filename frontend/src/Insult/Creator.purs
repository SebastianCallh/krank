module Insult.Creator where

import Prelude

import Audio.WebAudio.AudioBufferSourceNode (defaultStartOptions, setBuffer, startBufferSource)
import Audio.WebAudio.BaseAudioContext (createBufferSource, newAudioContext, destination)
import Audio.WebAudio.Types (AudioContext, AudioBuffer, connect)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.String.Common (toLower)
import Data.Time (Time, adjust)
import Data.Tuple (Tuple(..))
import Effect.Console (log)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, error, killFiber, launchAff)
import Effect.Now (nowTime)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Insult.Api (fetchAudio, saveInsult)
import Insult.Insult (Insult(..), Amount(..))
import Router (Route(..), routeFor)
import User (User, userName, allUsers)
import Web.UIEvent.MouseEvent (MouseEvent)

newtype Aud = Aud Unit
foreign import playAudio :: Unit -> Effect Aud
foreign import loadAudio :: Aud -> Effect Unit

data Query a
  = IncreaseInsult User User a
  | ShowMessage String a
  | HandleInput Input a
  | FetchAudio a
  
type Input = Maybe User

type State = 
  { msender      :: Maybe User
  , mmessage     :: Maybe String
  , statuses     :: Map (Tuple User User) InsultStatus
  , onClickAudio :: Maybe AudioBuffer
  , audioContext :: Maybe AudioContext
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
  H.lifecycleComponent
    { initialState: initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    , initializer: pure $ H.action FetchAudio
    , finalizer: Nothing
    }
  where
    initialState muser =
      { msender:  muser
      , mmessage: Nothing
      , statuses: M.empty
      , onClickAudio: Nothing
      , audioContext: Nothing
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
        userPanel userHref onClick state.statuses

    selectToList sender =
      let
        userHref = const $ routeFor $ SelectReceiver $ userName sender
        onClick receiver = HE.input_ $ IncreaseInsult sender receiver
      in
      userPanel userHref onClick state.statuses


userPanel
  :: forall t1 t2. (User -> String)
  -> (User -> MouseEvent -> Maybe t1)
  -> Map (Tuple User User) InsultStatus
  -> HH.HTML t2 t1
userPanel userHref userClicked statuses =
  HH.div [ HP.class_ $ HH.ClassName "user-panel"
         , HP.prop (H.PropName  "") ""
         ] userButtons
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
    maud <- H.gets _.onClickAudio
    mctx <- H.gets _.audioContext
    _    <- case Tuple maud mctx of
      Tuple (Just aud) (Just ctx) ->
        H.liftEffect $ do
          src <- createBufferSource ctx
          dst <- destination ctx
          _   <- connect src dst
          _   <- setBuffer aud src
          startBufferSource defaultStartOptions src

      _ -> do
        H.liftEffect $ log "Could not play sound. Context or audio was Nothing."
        pure unit
      
    let key = Tuple from to         
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

eval (FetchAudio next) = do
  ctx    <- H.liftEffect newAudioContext
  buffer <- H.liftAff $ fetchAudio ctx "air-horn.wav"
  H.modify_ _ { audioContext = pure ctx
              , onClickAudio = pure buffer
              }
  pure next
