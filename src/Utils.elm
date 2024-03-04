module Utils exposing (..)

import Http
import Json.Decode


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


decodeTuple : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder ( a, b )
decodeTuple decoder1 decoder2 =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.index 0 decoder1)
        (Json.Decode.index 1 decoder2)


decode3Tuple : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder c -> Json.Decode.Decoder ( a, b, c )
decode3Tuple decoder1 decoder2 decoder3 =
    Json.Decode.map3 (\a b c -> ( a, b, c ))
        (Json.Decode.index 0 decoder1)
        (Json.Decode.index 1 decoder2)
        (Json.Decode.index 2 decoder3)


map3TupleTo : (a -> b -> c -> d) -> (a, b, c) -> d
map3TupleTo f (a, b, c) =
    f a b c