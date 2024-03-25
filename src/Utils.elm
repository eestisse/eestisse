module Utils exposing (..)

import Element
import Html.Events
import Http
import Json.Decode
import Types exposing (..)


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


map3TupleTo : (a -> b -> c -> d) -> ( a, b, c ) -> d
map3TupleTo f ( a, b, c ) =
    f a b c


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )


isValidEmail : String -> Bool
isValidEmail input =
    let
        s1 =
            String.split "@" input

        ( a, bc ) =
            ( List.head s1, s1 |> List.tail |> Maybe.andThen List.head )
                |> Tuple.mapBoth (Maybe.withDefault "") (Maybe.withDefault "")

        s2 =
            String.split "." bc

        ( b, c ) =
            ( List.head s2, s2 |> List.tail |> Maybe.andThen List.head )
                |> Tuple.mapBoth (Maybe.withDefault "") (Maybe.withDefault "")
    in
    List.all (String.length >> (/=) 0)
        [ a, b, c ]
        && (List.length s1 == 2)


elementColorToRgb : Element.Color -> RGB
elementColorToRgb elColor =
    let
        rgba =
            Element.toRgb elColor
    in
    RGB
        rgba.red
        rgba.green
        rgba.blue
