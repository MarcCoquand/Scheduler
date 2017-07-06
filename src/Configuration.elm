module Configuration exposing (..)

import Date exposing (Date)
import Time exposing (Time)


type alias Config =
    { today : Date
    , withinDates : WithinDates
    , withinTimes : TimeOfDay
    , length : Int --hour
    , possibleTimes :
        List Int
    , weekDays : Bool
    , weekEnds : Bool
    }


type WithinDates
    = OneWeek
    | TwoWeeks
    | OneMonth
    | OneYear
    | CustomDate ( Date, Date )


type alias TimeInterval =
    ( Int, Int )


type alias TimeOfWeek =
    { weekDays : Bool
    , weekEnds : Bool
    }


type TimeOfDay
    = Morning
    | Lunch
    | Afternoon
    | Evening
    | CustomTime TimeInterval



--Easier date construction


day : Float
day =
    (24 * Time.hour)


week : Float
week =
    (7 * day)


extractTimes : TimeOfDay -> ( Int, Int )
extractTimes timeOfDay =
    ( getStartTime timeOfDay, getEndTime timeOfDay )


extractDates : Date -> WithinDates -> ( Date, Date )
extractDates today within =
    case within of
        CustomDate ( a, b ) ->
            ( a, b )

        OneWeek ->
            ( today, Date.fromTime <| Date.toTime today + week )

        TwoWeeks ->
            ( today, Date.fromTime <| Date.toTime today + 2 * week )

        OneMonth ->
            ( today, Date.fromTime <| Date.toTime today + 30 * day )

        OneYear ->
            ( today, Date.fromTime <| Date.toTime today + 366 * day )


setToday : Date -> Config -> Config
setToday today config =
    { config | today = today }


setDateRange : WithinDates -> Config -> Config
setDateRange within config =
    { config | withinDates = within }


setDayInterval : TimeOfDay -> Config -> Config
setDayInterval time config =
    { config | withinTimes = time }


getStartTime : TimeOfDay -> Int
getStartTime timeOfDay =
    case timeOfDay of
        Morning ->
            8

        Lunch ->
            12

        Afternoon ->
            13

        Evening ->
            17

        CustomTime ( start, _ ) ->
            start


getEndTime : TimeOfDay -> Int
getEndTime timeOfDay =
    case timeOfDay of
        Morning ->
            12

        Lunch ->
            13

        Afternoon ->
            17

        Evening ->
            22

        CustomTime ( _, end ) ->
            end
