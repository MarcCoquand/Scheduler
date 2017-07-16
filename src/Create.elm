module Create exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Html.Events exposing (onInput)
import Date exposing (..)
import Configuration exposing (..)
import Dater exposing (..)
import List exposing (..)


-- VIEW


radio : msg -> String -> Bool -> Html msg
radio msg name check =
    label []
        [ input [ type_ "radio", onClick msg, checked check ] []
        , text name
        ]


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name check =
    label
        []
        [ input [ type_ "checkbox", onClick msg, checked check ] []
        , text name
        ]


renderDayOfTime : Model -> Html Msg
renderDayOfTime model =
    div []
        [ timeOfDayBox "Morning" Morning model.config.withinTimes
        , timeOfDayBox "Lunch" Lunch model.config.withinTimes
        , timeOfDayBox "Afternoon" Afternoon model.config.withinTimes
        , timeOfDayBox "Evening" Evening model.config.withinTimes
        , checkbox Nop "Custom" False
        ]


timeOfDayBox : String -> TimeOfDay -> List TimeOfDay -> Html Msg
timeOfDayBox name tod list =
    checkbox (ToggleDayInterval tod) name (List.member tod list)


renderTimeOfWeek : Config -> Html Msg
renderTimeOfWeek config =
    div []
        [ checkbox
            (ToggleWeekInterval
                { config | weekDays = not config.weekDays }
            )
            "Weekday"
            True
        , checkbox
            (ToggleWeekInterval
                { config | weekEnds = not config.weekEnds }
            )
            "Weekend"
            False
        ]


checkWithinDate : Model -> WithinDates -> Bool
checkWithinDate model t =
    t == model.config.withinDates


renderDateInterval : Model -> Html Msg
renderDateInterval model =
    div []
        [ fieldset []
            [ radio (SwitchToDate OneWeek)
                "One week"
                (checkWithinDate model OneWeek)
            , radio (SwitchToDate TwoWeeks)
                "Two weeks"
                (checkWithinDate model TwoWeeks)
            , radio (SwitchToDate OneMonth)
                "One month"
                (checkWithinDate model OneMonth)
            , radio (SwitchToDate <| CustomDate ( (Date.fromTime 0), (Date.fromTime 0) ))
                "Select date"
                (checkWithinDate model <|
                    CustomDate ( (Date.fromTime 0), (Date.fromTime 0) )
                )
            ]
        ]


createLi : EasyDate -> Html Msg
createLi content =
    li [] [ text content.name, br [] [], text content.dateRange, br [] [], text content.timeRange ]


formatEvents : List ( String, Event ) -> List EasyDate
formatEvents list =
    case List.head list of
        Nothing ->
            []

        Just ( id, event ) ->
            case List.tail list of
                Nothing ->
                    [ Dater.formatAll event.name event.start event.end ]

                Just restOfTheList ->
                    [ Dater.formatAll event.name event.start event.end ] ++ formatEvents restOfTheList


renderADate : Model -> EasyDate -> Html Msg
renderADate model date = 
    div [] 
    [ text date.name
    , text date.dateRange
    ]

renderSuggestedDates : Model -> List EasyDate -> Html Msg
renderSuggestedDates model dateList = 
    div [] (List.map (renderADate model) <| take 3 dateList)

renderCreate : Model -> Html Msg
renderCreate model =
    div [ class "paper-container" ]
        [ div []
            [ div [ (class "create-header") ]
                [ h3 [] [ text "Book a meeting with " ]
                , input
                    [ placeholder "example@email.com"
                    , onInput NewMail
                    ]
                    []
                ]
            , div [ (class "input-container") ]
                [ h3 [] [ text "At which time?" ]
                , renderDayOfTime model
                ]
            , div [ (class "input-container") ]
                [ h3 [] [ text "On which days?" ]
                , renderTimeOfWeek model.config
                ]
            , div [ (class "input-container") ]
                [ h3 [] [ text "How soon?" ]
                , renderDateInterval model
                ]
            , div []
                [ text "Suggested dates: "
                , ul [] (List.map createLi (formatEvents model.filteredEvents))
                ]
            , div [ (class "input-container") ]
                [ button [] [ text "Send" ] ]
            ]
        ]
