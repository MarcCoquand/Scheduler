module Create exposing (..)

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


-- MODEL

-- UPDATE

-- VIEW

renderCreate : Model -> Html Msg
renderCreate model =
    div [] [text "hello"]
