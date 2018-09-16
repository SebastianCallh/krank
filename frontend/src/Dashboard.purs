module Dashboard where


import Prelude

import Data.Maybe (Maybe (..), maybe)
import Data.Argonaut (class DecodeJson, class EncodeJson, 
                      decodeJson, encodeJson, (.?), 
                      (:=), (~>), fromString, stringify)
import Affjax (get)
import Data.Either (Either (..))
import Affjax.ResponseFormat as ResponseFormat
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Effect.Aff (Aff)


data Query a
  = SelectSender User a
  | SendInsult User User a
  | ShowMessage String a


type Input = Unit

  
type State = 
  { msender :: Maybe User
  , message :: String
  }


newtype Insult = Insult
  { from   :: User
  , to     :: User
  , amount :: Int
  }


instance decodeInsult :: DecodeJson Insult where
  decodeJson json = do
    o      <- decodeJson json
    from   <- o .? "from"
    to     <- o .? "to"
    amount <- o .? "amount"
    pure $ Insult
          { from: from
          , to: to
          , amount: amount
          }


instance encodeInsult :: EncodeJson Insult where
  encodeJson (Insult i) =
    "from"   := encodeJson i.from ~>
    "to"     := encodeJson i.to   ~>
    "amount" := encodeJson i.amount
    

data User
    = Elin
    | Jimmie
    | Karl
    | Maria
    | Mike
    | Seba
    | Tintin

name :: User -> String
name Elin   = "Elin"
name Jimmie = "Jimmie"
name Karl   = "Karl"
name Maria  = "Maria"
name Mike   = "Mike"
name Seba   = "Seba"
name Tintin = "Tintin"


instance encodeUser :: EncodeJson User where
  encodeJson = name >>> fromString


instance decodeUser :: DecodeJson User where
  decodeJson user =
    case stringify user of
      "Elin"   -> Right Elin
      "Jimmie" -> Right Jimmie
      "Karl"   -> Right Karl
      "Maria"  -> Right Maria
      "Mike"   -> Right Mike
      "Seba"   -> Right Seba
      "Tintin" -> Right Tintin
      unknown  -> Left $
                  "Could not parse "
                  <> unknown
                  <> " as User."

  
dashboard :: H.Component HH.HTML Query Input Void Aff
dashboard =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState = { msender: Nothing, message: "" }


render :: State -> H.ComponentHTML Query
render state =
  case state.msender of
    Nothing ->
      HH.div [ ]
      [ HH.text "Vem..."
      , HH.hr_
      , selectFromList
      ]
      
    Just sender ->
      HH.div [ ]
      [ HH.text "Kränkte vem?"
      , HH.hr_
      , selectToList
      ]

  
  where
    selectFromList =
      userPanel SelectSender
          
    selectToList =
      let
        f = maybe (const $ ShowMessage "") SendInsult state.msender
      in
       userPanel f


userPanel :: forall t1 t2. (User -> Unit -> t1 Unit) -> HH.HTML t2 (t1 Unit)
userPanel f =
  HH.div [ HP.class_ $ HH.ClassName "user-panel"] userButtons 

  where
    userButtons =
      map userButton
      [ Elin
      , Jimmie
      , Karl
      , Maria
      , Mike
      , Seba
      , Tintin
      ]
                  
    userButton user =
      HH.a
      [ HP.class_ $ H.ClassName "user-button"
      , HE.onClick (HE.input_ $ f user)
      ]
      [ HH.text $ name user ]


eval :: Query ~> H.ComponentDSL State Query Void Aff
eval = case _ of
  SelectSender user next -> do
    H.modify_ $ \s -> s { msender = Just user }
    pure next
      
  SendInsult from to next -> do
    status <- H.gets _.msender
    case status of
      Nothing -> pure next
      Just user -> do          
          pure next

  ShowMessage msg next -> do
    H.modify_ $ \s -> s { message = msg }
    pure next
        
----- API STUFF


url :: String
url = "http://localhost:8081/insult"

fetchInsults :: Aff String
fetchInsults = do
  res <- get ResponseFormat.json url
  case res.body of
    Left  err  -> pure "bad"
--      log $ "GET /api response failed to decode: " <> AX.printResponseFormatError err
    Right insults -> pure "ok"
--      log $ "GET /api response: " <> J.stringify json
  
saveInsult :: Insult -> Aff Int
saveInsult newInsult = do pure 1
{-  res <- post ResponseFormat.json url (toJSON newInsult)
  case res.body of
    Left err -> pure 0
    Right insult -> pure 1-}
