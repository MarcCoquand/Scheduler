module Model exposing(..)

import Navigation exposing (Location)
import OAuth exposing (..)
import Calendar exposing (..)
import Dict exposing (..)


type alias Model =
    { message : String
    , calendars : Dict String String
    , events : List ( String, Calendar.Event )
    , working : Bool
    , token : Maybe OAuth.Token
    , route : Location
    }

