{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Db where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson
import           Data.Int
import           Data.Text
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics

import           Model.Person


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Insult
    from    Person
    to      Person
    amount  Int
    added   UTCTime
    deriving Generic
|]

run :: MonadIO m => SqlPersistT (ResourceT (NoLoggingT IO)) a -> m a
run = liftIO . runNoLoggingT . runResourceT . withSqliteConn dbFile . runSqlConn
  where dbFile = "insult.db"

migrate :: MonadIO m => ReaderT SqlBackend m ()
migrate = runMigration migrateAll
