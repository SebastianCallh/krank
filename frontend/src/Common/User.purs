module Common.User where


import Prelude 
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonNull, (.?), (:=), (~>))
import Data.Newtype (class Newtype)

----- NAME -----

newtype Name = Name String

derive instance newtypeName :: Newtype Name _

instance showName :: Show Name where
  show (Name n) = show n

instance encodeName :: EncodeJson Name where
  encodeJson (Name n) = encodeJson n

instance decodeName :: DecodeJson Name where
  decodeJson = map Name <<< decodeJson

----- PASSWORD -----

newtype Password = Password String

derive instance newtypePassword :: Newtype Password _

instance showPassword :: Show Password where
  show (Password p) = show p

instance encodePassword :: EncodeJson Password where
  encodeJson (Password p) = encodeJson p

instance decodePassword :: DecodeJson Password where
  decodeJson = map Password <<< decodeJson

------ USER -----

data User = User
  { name     :: Name
  } 

instance decodeUser :: DecodeJson User where
  decodeJson json = do
    o      <- decodeJson json
    name   <- o .? "name"
    pure $ User
      { name: name
      }

instance encodeUser :: EncodeJson User where
  encodeJson (User u) =
    "name"   := encodeJson u.name   ~>
    jsonNull
