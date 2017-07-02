module CalenderDecoder exposing (..)

import Json.Decode exposing (..)


a =
    decodeString (keyValuePairs string)
