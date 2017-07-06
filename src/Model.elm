module Model exposing (..)

import Navigation exposing (Location)
import OAuth exposing (..)
import Calendar exposing (..)
import Http exposing (..)
import Date exposing (..)
import Dater exposing (..)
import Time exposing (..)


type alias Model =
    { message : String
    , calendars : List ( String, String )
    , notYetFetchedEvents : List String --the ids
    , events : List ( String, Calendar.Event )
    , filteredEvents : List ( String, Calendar.Event )
    , working : Bool
    , token : Maybe OAuth.Token
    , route : Location
    , sendform : SendForm
    , timeconfig : List TimeOfDay
    , weekconfig : List TimeOfWeek
    , withindate : WithinTime
    , currentDate : Date
    }


type Msg
    = Nop
      --| FetchResponse (Result Http.Error String)
    | Token (Result Http.Error OAuth.Token)
    | UrlChange Location
    | GetCalendars
    | ShowCalendars (List ( String, String ))
    | ShowEvents (List ( String, Calendar.Event ))
    | NewMail String
    | ToggleDayInterval TimeOfDay
    | ToggleWeekInterval TimeOfWeek
    | SwitchToDate WithinTime
    | ShowFreeDates (List ( String, Event )) Dater.Config
    | RequestCurrentTime
    | UpdateTime Date



-- Interval during the day, example: 12:50 - 14:30


type alias TimeInterval =
    ( Time, Time )


type WithinTime
    = OneWeek
    | TwoWeeks
    | OneMonth
    | OneYear
    | CustomDate ( Date, Date )


type TimeOfWeek
    = Weekday
    | Weekend


type TimeOfDay
    = Morning
    | Lunch
    | Afternoon
    | Evening
    | CustomTime TimeInterval


type alias SendForm =
    { email : Maybe String
    , startDate : Maybe Date
    , endDate : Maybe Date
    }
