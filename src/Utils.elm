module Utils exposing (..)

import Element
import Html.Events
import Http
import Json.Decode
import List.Extra
import Random
import Time
import Types exposing (..)


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid."

        Http.Timeout ->
            "Connection timed out. Try again."

        Http.NetworkError ->
            "Unable to reach the server. Check your network connection and try again."

        Http.BadStatus num ->
            "HTTP \"BadStatus\": " ++ String.fromInt num

        Http.BadBody errorMessage ->
            "HTTP \"BadBody\": " ++ errorMessage


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


timeToRandomSeed : Time.Posix -> Random.Seed
timeToRandomSeed =
    Time.posixToMillis >> Random.initialSeed


interpolateColors : Float -> Element.Color -> Element.Color -> Element.Color
interpolateColors i color1 color2 =
    let
        rgba1 =
            Element.toRgb color1

        rgba2 =
            Element.toRgb color2
    in
    { red = interpolateFloats i rgba1.red rgba2.red
    , green = interpolateFloats i rgba1.green rgba2.green
    , blue = interpolateFloats i rgba1.blue rgba2.blue
    , alpha = interpolateFloats i rgba1.alpha rgba2.alpha
    }
        |> Element.fromRgb


interpolateFloats : Float -> Float -> Float -> Float
interpolateFloats progressFloat old new =
    old + ((new - old) * progressFloat)


list2ToTuple : List a -> Maybe ( a, a )
list2ToTuple l =
    case ( List.Extra.getAt 0 l, List.Extra.getAt 1 l ) of
        ( Just a, Just b ) ->
            Just ( a, b )

        _ ->
            Nothing


unixTimestampToPosix : Int -> Time.Posix
unixTimestampToPosix ts =
    (ts * 1000) |> Time.millisToPosix
