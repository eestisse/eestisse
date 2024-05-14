module Route exposing (..)

import Translation.Types exposing (..)
import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Landing
    | Translate
    | Admin
    | AuthCallback String
    | Account
    | StripeLinkback
    | Browse
    | History
    | View Int
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
        , Parser.map AuthCallback (Parser.s "login" </> Parser.string </> Parser.s "callback")
        , Parser.map Account (Parser.s "account")
        , Parser.map StripeLinkback (Parser.s "stripe-linkback")
        , Parser.map Browse (Parser.s "browse")
        , Parser.map History (Parser.s "history")
        , Parser.map View (Parser.s "view" </> Parser.int)
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

            AuthCallback methodId ->
                [ "login", methodId, "callback" ]

            Account ->
                [ "account" ]

            StripeLinkback ->
                [ "stripe-linkback" ]

            Browse ->
                [ "browse" ]

            History ->
                [ "history" ]

            View id ->
                [ "view", String.fromInt id ]

            BadRoute ->
                [ "badroute" ]
        )
        []


shouldRedirect : Route -> Bool
shouldRedirect route =
    case route of
        AuthCallback _ ->
            True

        _ ->
            False
