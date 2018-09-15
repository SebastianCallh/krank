module Main exposing (init, main, subscriptions)

import Browser
import Browser.Navigation as Nav exposing (Key)
import Data exposing (fetchPlayers)
import Data exposing (saveInsult)      
import Html exposing (Html, a, div, section, text, button)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Pages.Edit
import Pages.List
import Routes
import Shared exposing (..)
import Url exposing (Url)

type alias Flags =
    {}


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        currentRoute =
            Routes.parseUrl url
    in
    ( initialModel currentRoute key, fetchPlayers )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnFetchPlayers (Ok players) ->
            ( { model | players = Loaded players }, Cmd.none )

        OnFetchPlayers (Err err) ->
            ( { model | players = Failure }, Cmd.none )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        OnUrlChange url ->
            let
                newRoute =
                    Routes.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )

        ChangeLevel player howMuch ->
            let
                updatedPlayer =
                    { player | level = player.level + howMuch }
            in
            ( model, Data.savePlayerCmd updatedPlayer )

        OnPlayerSave (Ok player) ->
            ( updatePlayerInModel player model, Cmd.none )

        OnPlayerSave (Err error) ->
            ( model, Cmd.none )

        SendInsult from to amount ->
            ( model, (Data.saveInsult (Insult from to amount)) )

        OnInsultSaved result ->
            ( model, Cmd.none )

        OnInsultsFetched result ->
            ( model, Cmd.none )
            
        Login user ->
            ( { model | session = Just user }, Cmd.none )

updatePlayerInModel : Player -> Model -> Model
updatePlayerInModel player model =
    let
        updatedPlayers =
            mapRemoteData (updatePlayerInList player) model.players
    in
    { model | players = updatedPlayers }


updatePlayerInList : Player -> List Player -> List Player
updatePlayerInList player players =
    let
        pick currentPlayer =
            if currentPlayer.id == player.id then
                player

            else
                currentPlayer
    in
    List.map pick players



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }



-- VIEWS


view : Model -> Browser.Document Msg
view model =
    { title = "App"
    , body = [ page model ]
    }


page : Model -> Html Msg
page model =
    let
        content =
            case model.session of
                Nothing -> loginPanel

                Just person -> insultPanel person

{-
            case model.players of
                NotAsked ->
                    text ""

                Loading ->
                    text "Loading ..."

                Loaded players ->
                    pageWithData model players

                Failure ->
                    text "Error"-}
    in
    section []
        [ nav model
        , div [ class "p-4" ] [ content ]
        ]


pageWithData : Model -> List Player -> Html Msg
pageWithData model players =
    case model.route of
        PlayersRoute ->
            Pages.List.view players

        PlayerRoute id ->
            Pages.Edit.view players id

        NotFoundRoute ->
            notFoundView


nav : Model -> Html Msg
nav model =
    let
        links =
            case model.route of
                PlayersRoute ->
                    [ text "Players" ]

                PlayerRoute _ ->
                    [ linkToList
                    ]

                NotFoundRoute ->
                    [ linkToList
                    ]

        linkToList =
            a [ href Routes.playersPath, class "text-white" ] [ text "List" ]
    in
    div
        [ class "mb-2 text-white bg-black p-4" ]
        links


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]

        
loginPanel : Html Msg  
loginPanel =
    div []
        [ button [ onClick (Login Seba) ] [ text "Logga in" ]
        ]

        
insultPanel : User -> Html Msg
insultPanel from =
    div []
        [ button [ onClick (SendInsult from Elin 1) ] [ text "Seba" ]
        ]
        
