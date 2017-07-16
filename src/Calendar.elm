module Calendar exposing (..)

import OAuth exposing (..)
import OAuth.Config exposing (..)
import Http
import Json.Decode exposing (..)
import Date exposing (..)
import Json.Decode.Extra exposing (..)
import Configuration exposing (Event)


googleAuthClient : OAuth.Client
googleAuthClient =
    OAuth.newClient
        OAuth.Config.google
        { clientId = "285829109258-t4bqu0184ufsdesa8r1h8i0tgctgfp2f.apps.googleusercontent.com"
        , scopes = [ "https://www.googleapis.com/auth/calendar" ]
        , redirectUrl = "http://localhost:3000/book"
        , authFlow = OAuth.Implicit
        }


type alias ApiDef =
    { name : String
    , version : String
    , collection : String
    , method : String
    }


calendar : ApiDef
calendar =
    { name = "calendar"
    , version = "v3"
    , collection = "users/me/calendarList"
    , method = "get"
    }


events : String -> ApiDef
events calendarID =
    { name = "calendar"
    , version = "v3"
    , collection = "calendars/" ++ calendarID ++ "/events" --Todo, replace primary with calendarID
    , method = "get"
    }


apiKey : String
apiKey =
    "285829109258-t4bqu0184ufsdesa8r1h8i0tgctgfp2f"


getHeaders : OAuth.Token -> List Http.Header
getHeaders token =
    case token of
        OAuth.Validated t ->
            [ Http.header "Authorization" ("Bearer " ++ t) ]


req : OAuth.Token -> ApiDef -> List ( String, String ) -> Http.Request String
req token def fields =
    Http.request
        { method = def.method
        , headers = getHeaders token
        , url =
            url
                ("https://content.googleapis.com/" ++ def.name ++ "/" ++ def.version ++ "/" ++ def.collection)
                (fields ++ [ ( "key", apiKey ) ])
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


url : String -> List ( String, String ) -> String
url baseUrl args =
    case args of
        [] ->
            baseUrl

        _ ->
            baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    Http.encodeUri key ++ "=" ++ Http.encodeUri value


findCalendars : String -> List ( String, String )
findCalendars input =
    let
        ids =
            decodeString (field "items" (list (field "id" string))) <| input

        names =
            decodeString (field "items" (list (field "summary" string))) <| input
    in
        case ids of
            Err errorMessage ->
                []

            --nothing found
            Ok ids ->
                case names of
                    Ok names ->
                        List.map2 couple names ids

                    Err errorMessage ->
                        []


couple : String -> String -> ( String, String )
couple name id =
    ( name, id )


findEvents : String -> List ( String, Event )
findEvents input =
    let
        ids =
            decodeString (field "items" (list (field "id" string))) <| input

        names =
            decodeString (field "items" <| list <| maybe <| field "summary" string) <| input

        startTimes =
            decodeString (field "items" <| list <| maybe <| at [ "start", "dateTime" ] date) <| input

        endTimes =
            decodeString (field "items" <| list <| maybe <| at [ "end", "dateTime" ] date) <| input
    in
        case ids of
            Err errorMessage ->
                []

            Ok ids ->
                case startTimes of
                    Err errorMessage ->
                        []

                    Ok startTimes ->
                        case endTimes of
                            Err errorMessage ->
                                []

                            Ok endTimes ->
                                case names of
                                    Err errorMessage ->
                                        []

                                    Ok names ->
                                        filterOutNothings <| List.map4 makeEvent ids names startTimes endTimes


filterOutNothings : List (Maybe a) -> List a
filterOutNothings things =
    case List.head things of
        Nothing ->
            []

        Just a ->
            case a of
                Nothing ->
                    case List.tail things of
                        Nothing ->
                            []

                        Just rest ->
                            filterOutNothings rest

                Just a ->
                    case List.tail things of
                        Nothing ->
                            []

                        Just rest ->
                            [ a ] ++ filterOutNothings rest


makeEvent : String -> Maybe String -> Maybe Date -> Maybe Date -> Maybe ( String, Event )
makeEvent id name start end =
    case name of
        Nothing ->
            Nothing

        Just name ->
            case start of
                Nothing ->
                    Nothing

                Just start ->
                    case end of
                        Nothing ->
                            Nothing

                        Just end ->
                            Just
                                ( id
                                , { name = name, start = start, end = end }
                                )



-- combines two calendars and keeps sorting inteact--


combineTwoCalenders : List ( String, Event ) -> List ( String, Event ) -> List ( String, Event )
combineTwoCalenders list1 list2 =
    let
        first1 =
            List.head list1

        first2 =
            List.head list2

        rest1 =
            Maybe.withDefault [] (List.tail list1)

        rest2 =
            Maybe.withDefault [] (List.tail list2)
    in
        case first1 of
            Nothing ->
                list2

            Just ( id1, event1 ) ->
                case first2 of
                    Nothing ->
                        list1

                    Just ( id2, event2 ) ->
                        if (Date.toTime event1.start < Date.toTime event2.start) then
                            [ ( id1, event1 ) ] ++ combineTwoCalenders rest1 list2
                        else
                            [ ( id2, event2 ) ] ++ combineTwoCalenders list1 rest2


future : Date -> List ( String, Event ) -> List ( String, Event )
future currentDate events =
    case List.head events of
        Nothing ->
            []

        Just ( id, event ) ->
            if (Date.toTime event.end < Date.toTime currentDate) then
                future currentDate <| Maybe.withDefault [] (List.tail events)
            else
                events
