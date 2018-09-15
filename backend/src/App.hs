{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module App where
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Network.Wai.Middleware.Cors
import           Servant

import qualified Db
import           Model.Insult                (Insult, NewInsult)
import qualified Model.Insult                as Insult

type InsultAPI
  = "insult" :>
    (Get '[JSON] [Insult]
    :<|> ReqBody '[JSON] NewInsult :> Post '[JSON] Insult)

main :: IO ()
main = do
  Db.run Db.migrate
  putStrLn $ "Running backend on port " ++ show port ++ "!"
  withStdoutLogger $ \logger -> do
    let settings = setPort port $ setLogger logger defaultSettings
    runSettings settings app
  where
    port = 8081

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
