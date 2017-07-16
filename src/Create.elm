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
import Dater exposing (..)

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
        , radio (SwitchToDate <| CustomDate ((Date.fromTime 0), (Date.fromTime 0)))
            "Select date" (checkWithinDate model 
                <| CustomDate ((Date.fromTime 0), (Date.fromTime 0)))
        ]
    ]

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
    div [(class "paper-container")] 
        [ div [] 
            [ div [(class "create-header")] 
                [ h3 [] [text "Book a meeting with "]
                , input 
                    [ placeholder "example@email.com"
                    , onInput NewMail
                    ] []
                ]
            , div [(class "input-container")] 
                [ h3 [] [text "At which time?"]
                , renderDayOfTime model
                ]
            , div [(class "input-container")]
                [ h3 [] [text "On which days?"]
                , renderTimeOfWeek model
                ]
            , div [(class "input-container")]
                [ h3 [] [text "How soon?"]
                , renderDateInterval model
                ]
            , div [] 
                [ text "Suggested dates: " 
                , text "ADD DATES HERE"
                ]
            , div [(class "input-container")]
                [ button [] [text "Send"] ]
            ]
        ]
