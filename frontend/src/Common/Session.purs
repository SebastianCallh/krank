module Common.Session where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonNull, (.?), (:=), (~>))
import Common.User (User)
import Data.Newtype (class Newtype)

----- TOKEN -----

newtype Token = Token String

derive instance newtypeToken :: Newtype Token _

instance showToken :: Show Token where
  show (Token t) = show t

instance encodeToken :: EncodeJson Token where
  encodeJson (Token t) = encodeJson t

instance decodeToken :: DecodeJson Token where
  decodeJson = map Token <<< decodeJson

----- SESSION ------

data Session = Session
  { token :: Token
  , user  :: User
  }

instance decodeSession :: DecodeJson Session where
  decodeJson json = do
    o       <- decodeJson json
    token   <- o .? "token"
    user    <- o .? "user"
    pure $ Session
      { token: token
      , user:  user
      }

instance encodeSession :: EncodeJson Session where
  encodeJson (Session s) =
    "token"  := encodeJson s.token  ~>
    "user"   := encodeJson s.user  ~>
    jsonNull
