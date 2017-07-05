module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (..)
import Http
import Navigation exposing (Location)
import Calendar exposing (..)
import OAuth exposing (..)
import List exposing (..)
import Dater exposing (..)
import Model exposing (..)
import Create exposing (..)


-- MODEL


initSendForm : SendForm
initSendForm =
    { email = Nothing
    , startDate = Nothing
    , endDate = Nothing
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
        [ Morning ]
        [ Weekday ]
        OneWeek
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
            ( { model | token = Just t }, calendarCmd (Just t) )

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

        ShowEvents events ->
            ( { model
                | events = Calendar.combineTwoCalenders events model.events
                , notYetFetchedEvents = quickTail model.notYetFetchedEvents
              }
            , eventsCmd model.token (List.head model.notYetFetchedEvents)
            )

        NewMail mail ->
            ( { model | sendform = updateMail model.sendform mail }
            , Cmd.none
            )

        ToggleDayInterval timeofday ->
            if (List.member timeofday model.timeconfig) then
                ( { model
                    | timeconfig =
                        List.filter (\x -> x /= timeofday) model.timeconfig
                  }
                , Cmd.none
                )
            else
                ( { model | timeconfig = timeofday :: model.timeconfig }, Cmd.none )

        ToggleWeekInterval timeofweek ->
            if (List.member timeofweek model.weekconfig) then
                ( { model
                    | weekconfig =
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

        ShowFreeDates events config ->
            ( { model | events = Dater.freeDates events config }, Cmd.none )


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
                , p [] [ text <| toString model.token ]
                , button [ onClick GetCalendars ] [ text "click to load calendars" ]
                , p [] [ text <| model.message ]
                , renderCreate model
                , button [ onClick <| ShowFreeDates model.events Dater.testConfig ] [ text "hi" ]
                , ul [] (List.map createLi (formatEvents model.events))
                ]


createLi : EasyDate -> Html Msg
createLi content =
    li [] [ text content.name, br [] [], text content.dateRange, br [] [], text content.timeRange ]


formatEvents : List ( String, Event ) -> List EasyDate
formatEvents list =
    case head list of
        Nothing ->
            []

        Just ( id, event ) ->
            case tail list of
                Nothing ->
                    [ Dater.formatAll event.name event.start event.end ]

                Just restOfTheList ->
                    [ Dater.formatAll event.name event.start event.end ] ++ formatEvents restOfTheList



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
