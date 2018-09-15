module Shared exposing (Model, Msg(..), Insult, User(..), Player, PlayerId, RemoteData(..), Route(..), initialModel, mapRemoteData)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Url exposing (Url)


type User
    = Elin
    | Jimmie
    | Karl
    | Maria
    | Mike
    | Seba
    | Tintin

type alias Amount = Int

type alias Insult  =
    { from : User
    , to : User
    , amount : Int               
    }
    
type alias Model =
    { players : RemoteData (List Player)
    , session : Maybe User
    , key : Key
    , route : Route
    }


initialModel : Route -> Key -> Model
initialModel route key =
    { players = Loading
    , key = key
    , route = route
    , session = Nothing
    }


type alias PlayerId =
    String


type alias Player =
    { id : PlayerId
    , name : String
    , level : Int
    }


type Route
    = PlayersRoute
    | PlayerRoute PlayerId
    | NotFoundRoute



type Msg
    = OnFetchPlayers (Result Http.Error (List Player))
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | ChangeLevel Player Int
    | OnPlayerSave (Result Http.Error Player)
    | SendInsult User User Amount
    | OnInsultsFetched (Result Http.Error (List Insult))
    | OnInsultSaved (Result Http.Error String)
    | Login User


type RemoteData a
    = NotAsked
    | Loading
    | Loaded a
    | Failure


mapRemoteData : (a -> b) -> RemoteData a -> RemoteData b
mapRemoteData fn remoteData =
    case remoteData of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Loaded data ->
            Loaded (fn data)

        Failure ->
            Failure
