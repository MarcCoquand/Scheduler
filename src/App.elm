module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (..)
import Http
import Navigation exposing (Location)
import Calendar exposing (..)
import OAuth exposing (..)
import Dict exposing (..)
import List exposing (..)
import Model exposing (..)
import Create exposing (..)
import Date exposing (..)

-- MODEL

initSendForm = 
    { email     = Nothing
    , startDate = Nothing
    , endDate   = Nothing
    }

init : Location -> ( Model, Cmd Msg )
init location =
    ( Model.Model 
        "No message" 
        Dict.empty 
        [] 
        False 
        Nothing 
        location 
        initSendForm
        [Morning]
        [Weekday]
        OneWeek
    , OAuth.init googleAuthClient location |> Cmd.map Token
    )



-- UPDATE

updateMail : SendForm -> String -> SendForm
updateMail sendForm mail =
    { sendForm | email = Just mail }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --Do nothing
        Nop ->
            ( model, Cmd.none )

        {--Fetches
        FetchResponse (Result.Ok str) ->
            ( { model | message = str }, Cmd.none )

        FetchResponse (Result.Err (Http.BadStatus err)) ->
            if err.status.code == 404 then
                ( { model | message = message404 }, Cmd.none )
            else
                ( { model | message = toString err }, Cmd.none )

        FetchResponse (Result.Err err) ->
            ( { model | message = toString err }, Cmd.none )--}
        --Login token
        Token (Ok t) ->
            ( { model | token = Just t }, Cmd.none )

        Token (Err err) ->
            ( { model | message = toString err }, Cmd.none )

        --Routing
        UrlChange location ->
            ( { model | route = location }, Cmd.none )

        --Calendar
        GetCalendars ->
            ( model, calendarCmd model )

        GetEvents calendarID ->
            ( model, eventsCmd model calendarID )

        ShowCalendars calendars ->
            ( { model | calendars = calendars }, Cmd.none )

        ShowEvents events ->
            ( { model | events = events }, Cmd.none )

        NewMail mail ->
            ( {model |  sendform = updateMail model.sendform mail }
            , Cmd.none 
            )

        ToggleDayInterval timeofday ->
            if (List.member timeofday model.timeconfig) then 
                ( { model | timeconfig = 
                    List.filter (\x -> x /= timeofday) model.timeconfig 
                  }
                , Cmd.none )
            else 
                ( { model | timeconfig = timeofday :: model.timeconfig }, Cmd.none )

        ToggleWeekInterval timeofweek ->
            if (List.member timeofweek model.weekconfig) then 
                ( { model | weekconfig = 
                    List.filter (\x -> x /= timeofweek) model.weekconfig 
                  }
                , Cmd.none 
                )
            else 
                ( { model | weekconfig = timeofweek :: model.weekconfig }
                , Cmd.none 
                )

        SwitchToDate newDate ->
            ( { model | withindate = newDate }, Cmd.none )

message404 : String
message404 =
    "I got a 404."



-- VIEW


view : Model -> Html Msg
view model =
    div []
    [ h1 [] [ text "Quick meeting scheduler!" ]
    , p [] [ text <| toString model.calendars ]
    , a [ href <| OAuth.buildAuthUrl Calendar.googleAuthClient ] 
        [ text "google login" ]
    , p [] [ text <| toString model.token ]
    , button [ onClick GetCalendars ] [ text "click to load calendars" ]
    , eventButton model.token model.calendars
    , p [] [ text <| toString model.events ]
    , renderCreate model
    ]


eventButton : Maybe OAuth.Token -> Dict String String -> Html Msg
eventButton token calendars =
    let
        key =
            List.head <| Dict.keys calendars
    in
        case token of
            Nothing ->
                text "You have to be logged in"

            Just token ->
                case key of
                    Nothing ->
                        text "No calendars"

                    Just key ->
                        button [ onClick <| GetEvents key ] [ text key ]



-- COMMANDS


eventsCmd : Model -> String -> Cmd Msg
eventsCmd model calendarID =
    case model.token of
        Just token ->
            sendEventRequest token (events calendarID) []

        Nothing ->
            Cmd.none


calendarCmd : Model -> Cmd Msg
calendarCmd model =
    case model.token of
        Just token ->
            sendCalendarRequest token calendar []

        Nothing ->
            Cmd.none


findCalendars : Result Http.Error String -> Msg
findCalendars r =
    case r of
        Ok v ->
            ShowCalendars <| Calendar.findCalendars v

        Err _ ->
            Nop


findEvents : Result Http.Error String -> Msg
findEvents r =
    case r of
        Ok v ->
            ShowEvents <| Calendar.findEvents v

        Err _ ->
            Nop


sendCalendarRequest : 
    OAuth.Token -> Calendar.ApiDef -> List ( String, String ) -> Cmd Msg
sendCalendarRequest token def fields =
    Http.send findCalendars (req token def fields)


sendEventRequest : 
    OAuth.Token -> Calendar.ApiDef -> List ( String, String ) -> Cmd Msg
sendEventRequest token def fields =
    Http.send findEvents (req token def fields)



{--
loadData : Cmd Msg
loadData =
    Http.get "/api/default" (Json.field "data" Json.string)
    |> Http.send FetchResponse--}
