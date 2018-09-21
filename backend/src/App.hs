{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module App where
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Network.Wai.Middleware.Cors
import           Safe                        (readMay)
import           Servant
import           System.Environment          (lookupEnv)

import qualified Db
import           Model.Insult                (Insult, NewInsult)
import qualified Model.Insult                as Insult

type InsultAPI
  = "insult" :>
    (Get '[JSON] [Insult]
    :<|> ReqBody '[JSON] NewInsult :> Post '[JSON] Insult)

main :: IO ()
main = do
  port <- lookupSetting "PORT" 8081
  Db.run Db.migrate
  putStrLn $ "Running backend on port " ++ show port ++ "!"
  withStdoutLogger $ \logger -> do
    let settings = setPort port $ setLogger logger defaultSettings
    runSettings settings app


lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]

app :: Application
app = cors (const $ Just corsPolicy) $ serve insultAPI server
  where
    corsPolicy  = simpleCorsResourcePolicy
      { corsRequestHeaders = [ "authorization", "content-type" ]
      }

server :: Server InsultAPI
server = getInsults
  :<|> postInsult

postInsult :: NewInsult -> Handler Insult
postInsult = Insult.save

getInsults :: Handler [Insult]
getInsults = Insult.all

insultAPI :: Proxy InsultAPI
insultAPI = Proxy
