module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import App exposing (..)
import Model exposing (..)
import Navigation


main =
    Navigation.program
        (always Nop)
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
