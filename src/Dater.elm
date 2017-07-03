module Dater exposing (..)

import Date exposing (..)
import Time exposing (..)
import Tuple exposing (first)


--returns the difference between the two dates in minutes
--returns 09, 10, 00 instead of 9 10 0 so it looks nicer


formatInteger : Int -> String
formatInteger int =
    if (abs int < 10) then
        if (int < 0) then
            "-0" ++ toString (abs int)
        else
            "0" ++ toString (abs int)
    else
        toString int


formatAll : String -> Date -> Date -> EasyDate
formatAll name d1 d2 =
    { name = name
    , dateRange = formatDateRange d1 d2
    , timeRange = formatHourRange d1 d2
    , d1 = d1
    , d2 = d2
    }


diffMinutes : Date -> Date -> Int
diffMinutes d1 d2 =
    let
        time1 =
            toTime d1

        time2 =
            toTime d2
    in
        round <| abs (Time.inMinutes time1) - (Time.inMinutes time2)


diffHours : Date -> Date -> Int
diffHours d1 d2 =
    diffMinutes d1 d2 // 60


isOrder : Date -> Date -> Bool
isOrder d1 d2 =
    let
        time1 =
            toTime d1

        time2 =
            toTime d2
    in
        time1 < time2


order : Date -> Date -> ( Date, Date )
order d1 d2 =
    if isOrder d1 d2 then
        ( d1, d2 )
    else
        ( d2, d1 )


sameDate : Date -> Date -> Bool
sameDate d1 d2 =
    day d1 == day d2 && month d1 == month d2 && year d1 == year d2


formatDateRange : Date -> Date -> String
formatDateRange d1 d2 =
    if sameDate d1 d2 then
        formatDay d1
    else
        formatDay d1 ++ " - " ++ formatDay d2


formatDay : Date -> String
formatDay date =
    toString (year date) ++ " " ++ toString (month date) ++ " " ++ formatInteger (day date) ++ " (" ++ toString (dayOfWeek date) ++ ")"


formatHourRange : Date -> Date -> String
formatHourRange d1 d2 =
    let
        ( date1, date2 ) =
            order d1 d2
    in
        if sameDate date1 date2 then
            formatHour date1 ++ " - " ++ formatHour date2
        else
            toString date1 ++ " - " ++ toString date2


formatHour : Date -> String
formatHour date =
    formatInteger (Date.hour date) ++ ":" ++ formatInteger (Date.minute date)



--discovers if two things collide


overLaps : ( Date, Date ) -> ( Date, Date ) -> Bool
overLaps ( d1Start, d1End ) ( d2Start, d2End ) =
    let
        d1IsBefore =
            isOrder d1End d2Start

        d2IsBefore =
            isOrder d2End d1Start
    in
        xor d1IsBefore d2IsBefore


type alias EasyDate =
    { name : String
    , dateRange : String
    , timeRange : String
    , d1 : Date
    , d2 : Date
    }


type alias Config =
    { currentDate : Date
    , endDate : Date
    }


freeDates : List ( Date, Date ) -> Config -> List ( Date, Date )
freeDates events config =
    let
        event =
            List.head events

        currentDate =
            config.currentDate

        next =
            { config | currentDate = nextDay currentDate }
    in
        if (isOrder config.currentDate config.endDate) then
            case event of
                Nothing ->
                    [ nineToFive currentDate ] ++ freeDates events next

                Just event ->
                    if (sameDate currentDate (Tuple.first event)) then
                        freeDates events next
                    else if (isOrder currentDate (Tuple.first event)) then
                        [ nineToFive currentDate ] ++ freeDates events next
                    else
                        case List.tail events of
                            Nothing ->
                                [ nineToFive currentDate ] ++ freeDates [] next

                            Just restOfEvents ->
                                freeDates restOfEvents config
        else
            []


nineToFive : Date -> ( Date, Date )
nineToFive date =
    let
        dateAsTime =
            Date.toTime date

        start =
            Date.fromTime <| toFloat <| round (Time.inHours dateAsTime / 24) + round (9 * Time.hour)

        end =
            Date.fromTime <| toFloat <| round (Time.inHours dateAsTime / 24) + round (17 * Time.hour)
    in
        ( start, end )



--TODO


nextDay : Date -> Date
nextDay d =
    Date.fromTime <| (24 * Time.hour) + Date.toTime d
