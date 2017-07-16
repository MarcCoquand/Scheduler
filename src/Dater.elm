module Dater exposing (formatAll, freeDates, EasyDate, oneYearAgo)

import Date exposing (..)
import Time exposing (..)
import Maybe exposing (andThen)
import Configuration exposing (..)


--returns the difference between the two dates in minutes
--returns 09, 10, 00 instead of 9 10 0 so it looks nicer


type alias EasyDate =
    { name : String
    , dateRange : String
    , timeRange : String
    , d1 : Date
    , d2 : Date
    }


pickThree : List ( String, Event ) -> List ( String, Event )
pickThree list =
    let
        length =
            List.length list

        t =
            Date.fromTime 0

        msg =
            toString length
    in
        takeEvery 0 (length // 3) list


calculateLength : Int -> List a -> Int
calculateLength index list =
    case List.head list of
        Nothing ->
            index

        Just a ->
            case List.tail list of
                Nothing ->
                    index + 1

                Just rest ->
                    calculateLength (index + 1) rest


takeEvery : Int -> Int -> List a -> List a
takeEvery index nth list =
    if (index >= nth - 1) then
        case List.head list of
            Nothing ->
                []

            Just first ->
                case List.tail list of
                    Nothing ->
                        [ first ]

                    Just rest ->
                        [ first ] ++ takeEvery 0 nth rest
    else
        case List.tail list of
            Nothing ->
                []

            Just rest ->
                takeEvery (index + 1) nth rest



--The big one, filtering out dates where the user is occupied


freeDates : List ( String, Event ) -> Config -> List ( String, Event )
freeDates events config =
    pickThree <| freeDatesLoop events config


freeDatesLoop : List ( String, Event ) -> Config -> List ( String, Event )
freeDatesLoop events config =
    let
        first =
            List.head events

        rest =
            Maybe.withDefault [] (List.tail events)

        ( currentDay, endDay ) =
            Configuration.extractDates config.today config.withinDates

        dayAfter =
            { config | withinDates = CustomDate ( nextDay currentDay, endDay ) }

        dayOfWeekAllowed =
            (config.weekDays || isWeekEnd currentDay) && (config.weekEnds || isWeekDay currentDay)
    in
        if (isOrder currentDay endDay) then
            if dayOfWeekAllowed then
                case first of
                    Nothing ->
                        (findFreeTimes events config) ++ freeDatesLoop events dayAfter

                    Just ( key, event ) ->
                        if (sameDay currentDay event.start) then
                            (findFreeTimes events config) ++ freeDatesLoop events dayAfter
                        else if (isOrder currentDay event.start) then
                            (findFreeTimes events config) ++ freeDatesLoop events dayAfter
                        else
                            freeDatesLoop rest config
            else
                freeDatesLoop events dayAfter
        else
            []


findFreeTimes : List ( String, Event ) -> Config -> List ( String, Event )
findFreeTimes events config =
    let
        ( day, _ ) =
            Configuration.extractDates config.today config.withinDates

        possibleTimes =
            config.possibleTimes

        first =
            List.head possibleTimes

        rest =
            Maybe.withDefault [] (List.tail possibleTimes)

        freeTimes =
            checkPossibleTimes possibleTimes (getOnlyToday events) config
    in
        List.map (makeEvent "free at" config.length day) freeTimes


makeEvent : String -> Int -> Date -> Int -> ( String, Event )
makeEvent name length day start =
    let
        startAsDate =
            setTimeAtDate day (toFloat start)

        endAsDate =
            setTimeAtDate day (toFloat (start + length))
    in
        ( "id", Event name startAsDate endAsDate )


checkPossibleTimes : List Int -> List ( String, Event ) -> Config -> List Int
checkPossibleTimes possibleTimes events config =
    let
        time =
            List.head possibleTimes

        event =
            List.head events

        rest =
            Maybe.withDefault [] (List.tail possibleTimes)

        length =
            config.length
    in
        case time of
            Nothing ->
                []

            Just time ->
                case event of
                    Nothing ->
                        --if there are no more events this day
                        [ time ] ++ checkPossibleTimes rest [] config

                    Just ( key, event ) ->
                        if (sameTime ( time, time + length ) event) then
                            checkPossibleTimes rest events config
                        else
                            [ time ] ++ checkPossibleTimes rest events config



--assumes they are the same date, ints are in hours


sameTime : ( Int, Int ) -> Event -> Bool
sameTime ( startHour, endHour ) event =
    let
        eventStart =
            Date.hour event.start

        eventEnd =
            Date.hour event.end

        eventFirst =
            eventEnd > startHour

        otherFirst =
            endHour > eventStart
    in
        not <| xor eventFirst otherFirst


getOnlyToday : List ( String, Event ) -> List ( String, Event )
getOnlyToday events =
    let
        head =
            List.head events

        rest =
            Maybe.withDefault [] (List.tail events)

        next =
            List.head rest
    in
        case head of
            Nothing ->
                []

            Just ( key, event ) ->
                case next of
                    Nothing ->
                        [ ( key, event ) ]

                    Just ( nextKey, nextEvent ) ->
                        if sameDay event.start nextEvent.start then
                            [ ( key, event ) ] ++ getOnlyToday rest
                        else
                            [ ( key, event ) ]



--Formatting


formatAll : String -> Date -> Date -> EasyDate
formatAll name d1 d2 =
    { name = name
    , dateRange = formatDateRange d1 d2
    , timeRange = formatHourRange d1 d2
    , d1 = d1
    , d2 = d2
    }


formatInteger : Int -> String
formatInteger int =
    if (abs int < 10) then
        if (int < 0) then
            "-0" ++ toString (abs int)
        else
            "0" ++ toString (abs int)
    else
        toString int


formatDateRange : Date -> Date -> String
formatDateRange d1 d2 =
    if sameDay d1 d2 then
        formatDate d1
    else
        formatDate d1 ++ " - " ++ formatDate d2


formatDate : Date -> String
formatDate date =
    toString (Date.year date) ++ " " ++ toString (Date.month date) ++ " " ++ formatInteger (Date.day date) ++ " (" ++ toString (Date.dayOfWeek date) ++ ")"


formatHourRange : Date -> Date -> String
formatHourRange d1 d2 =
    let
        ( date1, date2 ) =
            order d1 d2
    in
        if sameDay date1 date2 then
            formatHour date1 ++ " - " ++ formatHour date2
        else
            toString date1 ++ " - " ++ toString date2


formatHour : Date -> String
formatHour date =
    formatInteger (Date.hour date) ++ ":" ++ formatInteger (Date.minute date)



--discovers if two potential meetings collide


overlaps : ( Date, Date ) -> ( Date, Date ) -> Bool
overlaps ( d1Start, d1End ) ( d2Start, d2End ) =
    let
        d1IsBefore =
            isOrder d1End d2Start

        d2IsBefore =
            isOrder d2End d1Start
    in
        xor d1IsBefore d2IsBefore



--Easier date construction


day : Float
day =
    (24 * Time.hour)


week : Float
week =
    (7 * day)


wholeDay : String -> Config -> ( String, Event )
wholeDay name config =
    let
        ( startDate, endDate ) =
            Configuration.extractDates config.today <| config.withinDates

        ( startTime, endTime ) =
            Configuration.extractTimes config.withinTimes
    in
        meeting name startDate startTime (endTime - startTime)


meeting : String -> Date -> Int -> Int -> ( String, Event )
meeting name date startHour length =
    let
        dateAsTime =
            Date.toTime date

        start =
            setTimeAtDate date <| toFloat startHour

        end =
            setTimeAtDate date <| toFloat (startHour + length)
    in
        ( "noID"
        , Configuration.Event name start end
        )



--transformations


toDays : Date -> Int
toDays date =
    let
        currentHour =
            toFloat <| Date.hour date

        currentMinute =
            toFloat <| Date.minute date
    in
        round <| Date.toTime date - (hourMinute currentHour currentMinute)


setTimeAtDate : Date -> Float -> Date
setTimeAtDate date hourOfDay =
    let
        currentHour =
            toFloat <| Date.hour date

        currentMinute =
            toFloat <| Date.minute date
    in
        Date.fromTime <| Date.toTime date - (hourMinute currentHour currentMinute) + (Time.hour * hourOfDay)


nextDay : Date -> Date
nextDay d =
    Date.fromTime <| day + Date.toTime d


hourMinute : Float -> Float -> Time
hourMinute hour minute =
    (Time.hour * hour) + (Time.minute * minute)



--querys--


isWeekEnd : Date -> Bool
isWeekEnd date =
    (Date.dayOfWeek date == Sat)
        || (Date.dayOfWeek date == Sun)


isWeekDay : Date -> Bool
isWeekDay date =
    not <| isWeekEnd date


oneYearAgo : Date -> Date
oneYearAgo date =
    Date.fromTime <| Date.toTime date - 365 * day


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


sameDay : Date -> Date -> Bool
sameDay d1 d2 =
    Date.day d1 == Date.day d2 && Date.month d1 == Date.month d2 && Date.year d1 == Date.year d2
