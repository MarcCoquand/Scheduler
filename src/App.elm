module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class)
import Html.Events exposing (..)
import Http
import Navigation exposing (Location)
import Calendar exposing (..)
import OAuth exposing (..)
import List exposing (..)
import Dater exposing (..)
import Model exposing (..)
import Create exposing (..)
import Date exposing (..)
import Task exposing (..)
import Configuration exposing (..)


-- MODEL


initSendForm : SendForm
initSendForm =
    { email = Nothing
    , startDate = Nothing
    , endDate = Nothing
    }


initConfig : Configuration.Config
initConfig =
    { today = Date.fromTime 0
    , withinDates = OneWeek
    , withinTimes = [ Afternoon ]
    , length = 3
    , possibleTimes = []
    , weekDays = True
    , weekEnds = False
    }


init : Location -> ( Model, Cmd Msg )
init location =
    ( Model.Model
        "No message"
        []
        --calenders, will be fetched from google
        []
        --notYetFetchedEvents, those calenders that have not been processed into events
        []
        --events, all appointments
        []
        --filtered events´
        False
        Nothing
        location
        initSendForm
        initConfig
    , OAuth.init googleAuthClient location |> Cmd.map Token
    )



-- UPDATE


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
            ( { model | token = Just t }
            , Cmd.batch ([ calendarCmd (Just t), Task.perform UpdateTime Date.now ])
            )

        Token (Err err) ->
            ( { model | message = toString err }, Cmd.none )

        --Routing
        UrlChange location ->
            ( { model | route = location }, Cmd.none )

        --Calendar
        GetCalendars ->
            ( model, calendarCmd model.token )

        ShowCalendars calendars ->
            ( { model
                | calendars = calendars
                , notYetFetchedEvents = quickTail (extractIDs calendars)
              }
            , eventsCmd model.token (List.head (extractIDs calendars))
            )

        --I am using a year ago as now because I do not have any events planned
        ShowEvents events ->
            ( { model
                | events = Calendar.future (Dater.oneYearAgo model.config.today) <| Calendar.combineTwoCalenders events model.events
                , notYetFetchedEvents = quickTail model.notYetFetchedEvents
              }
            , eventsCmd model.token (List.head model.notYetFetchedEvents)
            )

        NewMail mail ->
            ( { model | sendform = updateMail model.sendform mail }
            , Cmd.none
            )

        ToggleDayInterval timeOfDay ->
            let
                config =
                    Configuration.setTimeRange timeOfDay model.config
            in
                ( { model
                    | config = config
                    , filteredEvents = Dater.freeDates model.events config
                  }
                , Cmd.none
                )

        ToggleWeekInterval config ->
            ( { model | config = config, filteredEvents = Dater.freeDates model.events config }, Cmd.none )

        SwitchToDate withinDate ->
            let
                config =
                    Configuration.setDateRange withinDate model.config
            in
                ( { model | config = config, filteredEvents = Dater.freeDates model.events config }, Cmd.none )

        ShowFreeDates events config ->
            ( { model | filteredEvents = Dater.freeDates events config }, Cmd.none )

        RequestCurrentTime ->
            ( model, Task.perform UpdateTime Date.now )

        UpdateTime date ->
            ( { model | config = Configuration.setToday date model.config }, Cmd.none )


quickTail : List a -> List a
quickTail list =
    Maybe.withDefault [] <| List.tail list


extractIDs : List ( String, String ) -> List String
extractIDs list =
    (List.map (\( name, id ) -> id) list)


updateMail : SendForm -> String -> SendForm
updateMail sendForm mail =
    { sendForm | email = Just mail }


message404 : String
message404 =
    "I got a 404."



-- VIEW


view : Model -> Html Msg
view model =
    case model.token of
        Nothing ->
            div []
                [ h1 [] [ text "Welcome to quick meeting scheduler" ]
                , p [] [ text "Use your google calendar to make sure you do not double book" ]
                , a [ href <| OAuth.buildAuthUrl Calendar.googleAuthClient ]
                    [ text "google login" ]
                ]

        Just token ->
            div []
                [ h1 [] [ text "Quick meeting scheduler!" ]
                , text <| toString model.config.withinTimes
                , div []
                    [ Create.renderCreate model ]
                ]



-- COMMANDS


eventsCmd : Maybe OAuth.Token -> Maybe String -> Cmd Msg
eventsCmd token calendarID =
    case token of
        Just token ->
            case calendarID of
                Nothing ->
                    Cmd.none

                Just calendarID ->
                    sendEventRequest token (events calendarID) []

        Nothing ->
            Cmd.none


calendarCmd : Maybe OAuth.Token -> Cmd Msg
calendarCmd token =
    case token of
        Just token ->
            sendCalendarRequest token Calendar.calendar []

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


sendCalendarRequest : OAuth.Token -> Calendar.ApiDef -> List ( String, String ) -> Cmd Msg
sendCalendarRequest token def fields =
    Http.send findCalendars (req token def fields)


sendEventRequest : OAuth.Token -> Calendar.ApiDef -> List ( String, String ) -> Cmd Msg
sendEventRequest token def fields =
    Http.send findEvents (req token def fields)



{--
loadData : Cmd Msg
loadData =
    Http.get "/api/default" (Json.field "data" Json.string)
    |> Http.send FetchResponse--}
