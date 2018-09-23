module Insult.Api where

import Prelude

import Affjax (get, post, printResponseFormatError)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Audio.WebAudio.BaseAudioContext (decodeAudioDataAsync)
import Audio.WebAudio.Types (AudioBuffer, AudioContext)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Exception (error)
import Halogen as H
import Insult.Insult (Insult)

insultUrl :: String
insultUrl = "/insult"

staticUrl :: String
staticUrl = "/"

fetchInsults :: Aff (Either String (Array Insult))
fetchInsults = do
  res <- get ResponseFormat.json insultUrl
  case res.body of
    Left err -> do
      pure <<< Left $ printResponseFormatError err
    Right json -> do
      pure $ decodeJson json
      

saveInsult :: Insult -> Aff Int
saveInsult newInsult = do
  res <- post ResponseFormat.json insultUrl $ RequestBody.json (encodeJson newInsult)
  case res.body of
    Left err -> do
      H.liftEffect $ log $ "error"
      pure 0
    Right insult -> do
      H.liftEffect $ log $ "ok"
      pure 1

type FileName = String

fetchAudio :: AudioContext -> FileName -> Aff AudioBuffer
fetchAudio ctx fileName = do
  res <- get ResponseFormat.arrayBuffer $ staticUrl <> fileName    
  case res.body of
    Left err ->
      throwError $ error $ printResponseFormatError err
    Right body ->
      decodeAudioDataAsync ctx body

