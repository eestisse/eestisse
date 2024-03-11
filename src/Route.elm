module Route exposing (..)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = Translate
    | History
    | Badroute


parseUrl : Url -> Route
parseUrl url =
    case Parser.parse matchRoute url of
        Just route ->
            route

        Nothing ->
            Badroute


matchRoute : Parser (Route -> a) a
matchRoute =
    Parser.oneOf
        [ Parser.map Translate Parser.top
        , Parser.map Translate (Parser.s "translate")
        , Parser.map History (Parser.s "history")
        ]
