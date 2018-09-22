module Insult.Api where

import Prelude

import Affjax (get, post, printResponseFormatError)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Insult.Insult (Insult)


url :: String
url = "http://localhost:8081/insult"


fetchInsults :: Aff (Either String (Array Insult))
fetchInsults = do
  res <- get ResponseFormat.json url
  case res.body of
    Left err -> do
      pure <<< Left $ printResponseFormatError err
    Right json -> do
      pure $ decodeJson json
      

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
