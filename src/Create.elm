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

-- VIEW
checkbox : msg -> String -> Html msg
checkbox msg name =
  label
    []
    [ input [ type_ "checkbox", onClick msg ] []
    , text name
    ]

renderDayOfTime : Model -> Html Msg
renderDayOfTime model =
    div [] 
    [ checkbox Nop "Morning"
    , checkbox Nop "Afternoon"
    , checkbox Nop "Lunch"
    , button [] [text "Cusom"]
    ]

renderCreate : Model -> Html Msg
renderCreate model =
    div [] 
        [ div [] 
            [ div [] 
                [ h3 [] [text "Book a meeting with:"]
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
                [ text "in "
                ]
            , div []
                [ button [] [text "Send"] ]
            ]
        ]
