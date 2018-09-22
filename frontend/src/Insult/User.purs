module User where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, fromString, stringify, toString)
import Data.Either (Either(..))
import Data.Maybe (maybe)

data User
    = Elin
    | Jimmie
    | Karl
    | Maria
    | Mike
    | Seba
    | Tintin

instance showUser :: Show User where
  show = userName
  
userName :: User -> String
userName Elin   = "Elin"
userName Jimmie = "Jimmie"
userName Karl   = "Karl"
userName Maria  = "Maria"
userName Mike   = "Mike"
userName Seba   = "Seba"
userName Tintin = "Tintin"

fromUserName :: String -> Either String User
fromUserName "Elin"   = Right Elin
fromUserName "Jimmie" = Right Jimmie
fromUserName "Karl"   = Right Karl
fromUserName "Maria"  = Right Maria
fromUserName "Mike"   = Right Mike
fromUserName "Seba"   = Right Seba
fromUserName "Tintin" = Right Tintin
fromUserName unknown  = Left $
  "Unknown username " <> unknown


instance encodeUser :: EncodeJson User where
  encodeJson = userName >>> fromString


instance decodeUser :: DecodeJson User where
  decodeJson json =
    maybe errorMsg fromUserName $ toString json
    where
      errorMsg = Left $
        "Could not parse json " <>
        stringify json <>
        " as string"
        

allUsers :: Array User
allUsers =
  [ Elin
  , Jimmie
  , Karl
  , Maria
  , Mike
  , Seba
  , Tintin
  ]


  
