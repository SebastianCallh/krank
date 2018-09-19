{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.Insult
  ( Insult
  , NewInsult
  , new
  , id
  , added
  , from
  , to
  , amount
  , save
  , all
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Time
import           Database.Persist
import           Database.Persist.Sqlite
import           GHC.Generics            (Generic)
import           Prelude                 hiding (all, id)

import qualified Db
import           Model.Person

data Insult = Insult
  { id     :: !Int
  , added  :: !UTCTime
  , from   :: !Person
  , to     :: !Person
  , amount :: !Int
  } deriving Generic

instance FromJSON Insult
instance ToJSON Insult

data NewInsult = NewInsult
  { from'   :: !Person
  , to'     :: !Person
  , amount' :: !Int
  } deriving Generic

instance FromJSON NewInsult where
  parseJSON (Object v) = NewInsult
    <$> v .: "from"
    <*> v .: "to"
    <*> v .: "amount"
  parseJSON _          = mzero

instance ToJSON NewInsult

new
  :: Person
  -> Person
  -> Int
  -> NewInsult
new = NewInsult


save :: MonadIO m => NewInsult -> m Insult
save newInsult@NewInsult { from', to', amount'} = do
  time <- liftIO getCurrentTime
  key  <- Db.run $ insert $ toDb time newInsult
  pure Insult
    { id     = keyToInt key
    , added  = time
    , from   = from'
    , to     = to'
    , amount = amount'
    }

all :: MonadIO m => m [Insult]
all = do
  entities <- Db.run $ selectList [] []
  pure $ fromDb <$> entities

fromDb :: Entity Db.Insult -> Insult
fromDb (Entity key dbInsult) = Insult
  (keyToInt key)
  (Db.insultAdded dbInsult)
  (Db.insultFrom dbInsult)
  (Db.insultTo dbInsult)
  (Db.insultAmount dbInsult)


toDb :: UTCTime -> NewInsult -> Db.Insult
toDb time f = Db.Insult
  (from' f)
  (to' f)
  (amount' f)
  time

keyToInt :: Key Db.Insult -> Int
keyToInt = fromIntegral . fromSqlKey
