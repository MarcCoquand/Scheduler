module Model exposing(..)

import Navigation exposing (Location)
import OAuth exposing (..)
import Calendar exposing (..)
import Dict exposing (..)
import Http exposing (..)


type alias Model =
    { message : String
    , calendars : Dict String String
    , events : List ( String, Calendar.Event )
    , working : Bool
    , token : Maybe OAuth.Token
    , route : Location
    }

type Msg
    = Nop
    --| FetchResponse (Result Http.Error String)
    | Token (Result Http.Error OAuth.Token)
    | UrlChange Location
    | GetEvents String
    | GetCalendars
    | ShowCalendars (Dict String String)
    | ShowEvents (List ( String, Calendar.Event ))

