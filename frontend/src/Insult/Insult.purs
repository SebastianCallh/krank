module Insult.Insult where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonNull, stringify, (.?), (:=), (~>))
import User (User)

newtype Insult = Insult
  { from   :: User
  , to     :: User
  , amount :: Int
  }

instance showInsult :: Show Insult where
  show = encodeJson >>> stringify

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
    "from"   := encodeJson i.from   ~>
    "to"     := encodeJson i.to     ~>
    "amount" := encodeJson i.amount ~>
    jsonNull
