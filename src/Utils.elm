module Utils exposing (..)

import Json.Decode
import Http


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

        Http.BadBody errorMessage ->
            errorMessage


decodeTuple : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder (a, b)
decodeTuple decoder1 decoder2 =
    Json.Decode.map2 Tuple.pair 
        (Json.Decode.index 0 decoder1)
        (Json.Decode.index 1 decoder2)