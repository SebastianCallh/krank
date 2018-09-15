module Data exposing (fetchInsults, saveInsult, fetchPlayers, savePlayerRequest, savePlayerCmd)

import Http
import Json.Decode exposing (Decoder, succeed, list, string, int, andThen, fail)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Shared exposing (..)


baseUrl : String
baseUrl = "http://localhost:8081"

insultsUrl : String
insultsUrl = baseUrl ++ "/insult"
         
fetchInsults : Cmd Msg
fetchInsults =
    Http.get insultsUrl insultsDecored
        |> Http.send OnInsultsFetched

saveInsult : Insult -> Cmd Msg
saveInsult insult = saveInsultReq insult
        |> Http.send OnInsultSaved
           
saveInsultReq : Insult -> Http.Request String
saveInsultReq insult =
    Http.request
        { body = insultEncoder insult |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = insultsUrl
        , withCredentials = False
        }
    
fetchPlayers : Cmd Msg
fetchPlayers =
    Http.get fetchPlayersUrl playersDecoder
        |> Http.send OnFetchPlayers


fetchPlayersUrl : String
fetchPlayersUrl =
    "http://localhost:4000/players"


savePlayerUrl : PlayerId -> String
savePlayerUrl playerId =
    "http://localhost:4000/players/" ++ playerId


savePlayerRequest : Player -> Http.Request Player
savePlayerRequest player =
    Http.request
        { body = playerEncoder player |> Http.jsonBody
        , expect = Http.expectJson playerDecoder
        , headers = []
        , method = "PATCH"
        , timeout = Nothing
        , url = savePlayerUrl player.id
        , withCredentials = False
        }


savePlayerCmd : Player -> Cmd Msg
savePlayerCmd player =
    savePlayerRequest player
        |> Http.send OnPlayerSave



-- DECODERS


playersDecoder : Decoder (List Player)
playersDecoder =
    list playerDecoder


playerDecoder : Decoder Player
playerDecoder =
    succeed Player
        |> required "id" string
        |> required "name" string
        |> required "level" int


playerEncoder : Player -> Encode.Value
playerEncoder player =
    let
        attributes =
            [ ( "id", Encode.string player.id )
            , ( "name", Encode.string player.name )
            , ( "level", Encode.int player.level )
            ]
    in
    Encode.object attributes

        
insultsDecored : Decoder (List Insult)
insultsDecored =
    list insultDecoder

        
insultEncoder : Insult -> Encode.Value
insultEncoder insult =
    Encode.object
        [ ("from", userEncoder insult.from)
        , ("to", userEncoder insult.from)
        , ("amount", Encode.int insult.amount)
        ]
        
            
insultDecoder : Decoder Insult
insultDecoder =
    succeed Insult
        |> required "from" (string |> andThen userDecoder) 
        |> required "to"   (string |> andThen userDecoder)
        |> required "amount" int
           

userEncoder : User -> Encode.Value
userEncoder user =
    Encode.string
    <| case user of
           Elin   -> "Elin"
           Jimmie -> "Jimmie"
           Karl   -> "Karl"
           Maria  -> "Maria"
           Mike   -> "Mike"
           Seba   -> "Seba"
           Tintin -> "Tintin"
    

userDecoder: String -> Decoder User
userDecoder val = 
  case val of
      "Elin"   -> succeed Elin
      "Jimmie" -> succeed Jimmie
      "Karl"   -> succeed Karl
      "Maria"  -> succeed Maria
      "Mike"   -> succeed Mike
      "Seba"   -> succeed Seba
      "Tintin" -> succeed Tintin
      unknown  -> fail <| "Could not parse " ++ unknown ++ " as user."
