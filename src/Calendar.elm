module Calendar exposing (..)

import OAuth exposing (..)
import OAuth.Config exposing (..)
import Http
import Json.Decode exposing (..)
import Dict exposing (..)


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
    , collection = "calendars/" ++ "primary" ++ "/events" --Todo, replace primary with calendarID
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


findCalendars : String -> Dict String String
findCalendars input =
    let
        ids =
            decodeString (field "items" (list (field "id" string))) <| input

        names =
            decodeString (field "items" (list (field "summary" string))) <| input
    in
        case ids of
            Err errorMessage ->
                Dict.fromList []

            --nothing found
            Ok ids ->
                case names of
                    Err errorMessage ->
                        Dict.fromList []

                    Ok names ->
                        Dict.fromList <| List.map2 couple ids names


couple : String -> String -> ( String, String )
couple id name =
    ( id, name )


findEvents : String -> Dict String Event
findEvents input =
    let
        ids =
            decodeString (field "items" (list (field "id" string))) <| input

        names =
            decodeString (field "items" <| list <| maybe <| field "summary" string) <| input

        startTimes =
            decodeString (field "items" <| list <| maybe <| at [ "start", "dateTime" ] string) <| input

        endTimes =
            decodeString (field "items" <| list <| maybe <| at [ "start", "dateTime" ] string) <| input
    in
        case ids of
            Err errorMessage ->
                Dict.fromList []

            Ok ids ->
                case startTimes of
                    Err errorMessage ->
                        Dict.singleton "errorID" <| errorEvent errorMessage

                    Ok startTimes ->
                        case endTimes of
                            Err errorMessage ->
                                Dict.singleton "errorID" <| errorEvent errorMessage

                            Ok endTimes ->
                                case names of
                                    Err errorMessage ->
                                        Dict.singleton "errorID" <| errorEvent errorMessage

                                    Ok names ->
                                        Dict.fromList <| filterOutNothings <| List.map4 makeEvent ids names startTimes endTimes


type alias Event =
    { name : Maybe String
    , start : Maybe String
    , end : Maybe String
    }


errorEvent : String -> Event
errorEvent message =
    { name = Just message, start = Just "..", end = Just ".." }


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


makeEvent : String -> Maybe String -> Maybe String -> Maybe String -> Maybe ( String, Event )
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
                                , { name = Just name, start = Just start, end = Just end }
                                )



{--
    makeUnnamedEvent : String -> String -> String -> ( String, Event )
    makeUnnamedEvent id start end =
    ( id
    , { name = Just "no name", start = Just start, end = Just end }
    )
--}