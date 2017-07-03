module Create exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Navigation exposing (Location)
import Calendar exposing (..)
import OAuth exposing (..)
import Dict exposing (..)
import List exposing (..)
import Model exposing (..)
import Html.Events exposing (onInput)
import Date exposing (..)

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
    [ input [type_ "checkbox", onClick msg, checked check] []
    , text name
    ]

renderDayOfTime : Model -> Html Msg
renderDayOfTime model =
    div [] 
    [ checkbox (ToggleDayInterval Morning) "Morning" True
    , checkbox (ToggleDayInterval Afternoon) "Afternoon" False
    , checkbox (ToggleDayInterval Lunch) "Lunch" False
    , checkbox Nop "Custom" False
    ]


renderTimeOfWeek : Model -> Html Msg
renderTimeOfWeek model =
    div []
    [ checkbox (ToggleWeekInterval Weekday) "Weekday" True
    , checkbox (ToggleWeekInterval Weekend) "Weekend" False
    ]


checkWithinDate : Model -> WithinTime -> Bool
checkWithinDate model t = 
    t == model.withindate

renderDateInterval : Model -> Html Msg
renderDateInterval model = 
    div []
    [ fieldset []
        [ radio (SwitchToDate OneWeek) "One week" 
            (checkWithinDate model OneWeek)
        , radio (SwitchToDate TwoWeeks) "Two weeks" 
            (checkWithinDate model TwoWeeks)
        , radio (SwitchToDate OneMonth) "One month" 
            (checkWithinDate model OneMonth)
        , radio (SwitchToDate OneYear) "One year" 
            (checkWithinDate model OneYear)
        , radio (SwitchToDate <| CustomDate ((Date.fromTime 0), (Date.fromTime 0)))
            "Select date" (checkWithinDate model 
                <| CustomDate ((Date.fromTime 0), (Date.fromTime 0)))
        ]
    ]

renderCreate : Model -> Html Msg
renderCreate model =
    div [] 
        [ div [] 
            [ div [] 
                [ h3 [] [text "Book a meeting with "]
                , input 
                    [ placeholder "Example@email.com"
                    , onInput NewMail
                    ] []
                ]
            , div [] 
                [ text "at "
                , renderDayOfTime model
                ]
            , div []
                [ text "on a "
                , renderTimeOfWeek model
                ]
            , div []
                [ text "in "
                , renderDateInterval model
                ]
            , div [] 
                [ text "Suggested dates: " 
                , text "ADD DATES HERE"
                ]
            , div []
                [ button [] [text "Send"] ]
            ]
        ]
