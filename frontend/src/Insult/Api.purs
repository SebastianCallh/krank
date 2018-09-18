module Insult.Api where

import Prelude

import Affjax (get, post)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (encodeJson, stringify)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Insult.Insult (Insult)


url :: String
url = "http://localhost:8081/insult"


fetchInsults :: Aff String
fetchInsults = do
  res <- get ResponseFormat.json url
  case res.body of
    Left err -> do
      H.liftEffect $ log $ "GET /api response failed to decode: "
      pure $ "err"
    Right json -> do
      H.liftEffect $ log $ "GET /api response: " <> stringify json
      pure $ stringify json
      
saveInsult :: Insult -> Aff Int
saveInsult newInsult = do
  res <- post ResponseFormat.json url $ RequestBody.json (encodeJson newInsult)
  case res.body of
    Left err -> do
      H.liftEffect $ log $ "error"
      pure 0
    Right insult -> do
      H.liftEffect $ log $ "ok"
      pure 1
