{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Model.Person where

import           Data.Aeson
import           Database.Persist.TH
import           GHC.Generics

data Person
    = Elin
    | Jimmie
    | Karl
    | Maria
    | Mike
    | Seba
    | Tintin
    deriving (Show, Read, Generic)

instance FromJSON Person
instance ToJSON Person

derivePersistField "Person"
