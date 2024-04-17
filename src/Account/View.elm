module Account.View exposing (..)

import Auth.Common exposing (ToBackend(..))
import CommonView exposing (..)
import Element exposing (Element)
import Responsive exposing (..)
import Types exposing (..)


page : DisplayProfile -> FrontendModel -> Element FrontendMsg
page dProfile model =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.spacing 20
        ]
        [ case model.authedUserEmail of
            Nothing ->
                mainActionButton "login with google oauth" <|
                    Just <|
                        AuthSigninRequested { methodId = "OAuthGoogle", username = Nothing }

            Just email ->
                Element.row []
                    [ Element.text <| "Logged in as " ++ email
                    , mainActionButton "log out" <|
                        Just <|
                            Logout
                    ]
        , Element.row
            [ Element.spacing 10 ]
            [ mainActionButton "How much do you like me??" <|
                Just <|
                    AskHowMuchYouLikeMe
            , viewHowMuchYouLikeMe model.backendModelAffection
            ]
        , case model.authedUserEmail of
            Just email ->
                mainActionButton "take my money" <| Just <| TriggerStripePayment email

            Nothing ->
                Element.none
        ]


viewHowMuchYouLikeMe : Maybe String -> Element FrontendMsg
viewHowMuchYouLikeMe maybeAffection =
    maybeAffection
        |> Maybe.withDefault "idk"
        |> Element.text
