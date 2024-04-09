module Route exposing (..)

import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Translate
    | Landing
    | Admin
    | Auth String
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
        [ Parser.map Landing Parser.top
        , Parser.map Translate (Parser.s "translate")
        , Parser.map Admin (Parser.s "admin")
        , Parser.map Auth (Parser.s "login" </> Parser.string </> Parser.s "callback")
        ]


routeToString : Route -> String
routeToString route =
    Url.Builder.absolute
        (case route of
            Translate ->
                [ "translate" ]

            Landing ->
                []

            Admin ->
                [ "admin" ]

            Auth methodId ->
                [ "login", methodId ]

            BadRoute ->
                [ "badroute" ]
        )
        []
