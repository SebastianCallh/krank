module Insult.Insult where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonNull, stringify, (.?), (:=), (~>))
import Data.Newtype (class Newtype, over2)
import User (User)

newtype Amount = Amount Int

derive instance newtypeAmount :: Newtype Amount _

instance semringAmount :: Semiring Amount where
  add  = over2 Amount add
  zero = Amount 0
  mul  = over2 Amount mul
  one  = Amount 1

instance showAmount :: Show Amount where
  show (Amount a) = show a

instance encodeAmount :: EncodeJson Amount where
  encodeJson (Amount a) = encodeJson a

instance decodeAmount :: DecodeJson Amount where
  decodeJson = map Amount <<< decodeJson
  
newtype Insult = Insult
  { from   :: User
  , to     :: User
  , amount :: Amount
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
