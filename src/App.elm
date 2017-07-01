module App exposing (init, update, view)

import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Json
import OAuth exposing (..)


-- MODEL


type alias Model =
    { message : String
    , working : Bool
    , user    : Maybe Client
    }


init : ( Model, Cmd Msg )
init =
    ( Model "Loading..." False Nothing
    , loadData
    )



-- UPDATE


type Msg
    = FetchResponse (Result Http.Error String)
    | LogIn Client


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchResponse (Result.Ok str) ->
            Model str True Nothing ! []

        FetchResponse (Result.Err (Http.BadStatus err)) ->
            if err.status.code == 404 then
                Model message404 True Nothing ! []
            else
                Model (toString err) False Nothing ! []

        FetchResponse (Result.Err err) ->
            Model (toString err) False Nothing ! []

        LogIn client -> 
          ( {model | user = Just client}
          , Cmd.none
          )
          


message404 =
    "I got a 404."



-- VIEW


view : Model -> Html Msg
view { message, working } =
    div []
        [ h1 [] [ text "Quick meeting scheduler!" ]
        , p [] [ text message ]
        , if working then
            div []
                [ text "I've loaded ! "
                ]
          else
            text ""
        ]



-- COMMANDS


loadData : Cmd Msg
loadData =
    Http.get "/api/default" (Json.field "data" Json.string)
        |> Http.send FetchResponse
