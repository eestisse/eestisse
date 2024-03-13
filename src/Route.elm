module Route exposing (..)

import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing (Parser)


type Route
    = Translate
    | About
    | BadRoute


parseUrl : Url -> Route
parseUrl url =
    case Parser.parse matchRoute url of
        Just route ->
            route

        Nothing ->
            BadRoute


matchRoute : Parser (Route -> a) a
matchRoute =
    Parser.oneOf
        [ Parser.map Translate Parser.top
        , Parser.map Translate (Parser.s "translate")
        , Parser.map About (Parser.s "about")
        ]


routeToString : Route -> String
routeToString route =
    Url.Builder.absolute
        (case route of
            Translate ->
                [ "translate" ]

            About ->
                [ "about" ]

            BadRoute ->
                [ "badroute" ]
        )
        []
