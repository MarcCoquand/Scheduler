module Configuration exposing (..)

import Date exposing (Date)
import Time exposing (Time)


type alias Config =
    { today : Date
    , withinDates : WithinDates
    , withinTimes : List TimeOfDay
    , length : Int --hour
    , possibleTimes :
        List Int
    , weekDays : Bool
    , weekEnds : Bool
    }


type alias Event =
    { name : String
    , start : Date
    , end : Date
    }


type WithinDates
    = OneWeek
    | TwoWeeks
    | OneMonth
    | OneYear
    | CustomDate ( Date, Date )


type alias TimeOfWeek =
    { weekDays : Bool
    , weekEnds : Bool
    }


type alias TimeInterval =
    ( Int, Int )


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


extractTimes : List TimeOfDay -> ( Int, Int )
extractTimes timesOfDay =
    let
        start =
            List.minimum <| List.map getStartTime timesOfDay

        end =
            List.maximum <| List.map getEndTime timesOfDay
    in
        case start of
            Nothing ->
                ( 10000, 10000 )

            Just start ->
                case end of
                    Nothing ->
                        ( start, 0 )

                    Just end ->
                        ( start, end )


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


setTimeRange : TimeOfDay -> Config -> Config
setTimeRange timeOfDay config =
    let
        isInList =
            List.member timeOfDay config.withinTimes

        newList =
            removeOrAdd isInList timeOfDay config.withinTimes
    in
        { config
            | withinTimes = newList
            , possibleTimes = List.sort <| List.concatMap (\tod -> getPossibleTimes (getStartTime tod) (getEndTime tod) config.length) newList
        }


getPossibleTimes : Int -> Int -> Int -> List Int
getPossibleTimes start end length =
    if (start > end - length) then
        []
    else
        [ start ] ++ getPossibleTimes (start + 1) end length


removeOrAdd : Bool -> a -> List a -> List a
removeOrAdd rem item list =
    if (rem) then
        remove item list
    else
        [ item ] ++ list



{--if isInList then
            { config | withinTimes = remove timeOfDay config.withinTimes }
        else
            { config | withinTimes = config.withinTimes ++ config.withinTimes }--}


remove : a -> List a -> List a
remove item list =
    case List.head list of
        Nothing ->
            []

        Just first ->
            if (first == item) then
                Maybe.withDefault [] (List.tail list)
            else
                [ first ] ++ (remove item <| Maybe.withDefault [] (List.tail list))


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
