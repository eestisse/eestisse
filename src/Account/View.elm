module Account.View exposing (..)

import Colors
import CommonView exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Responsive exposing (..)
import Types exposing (..)


page : DisplayProfile -> Maybe FrontendUserInfo -> Element FrontendMsg
page dProfile maybeUserInfo =
    primaryBox
        [ Element.width Element.fill
        , Element.padding <| responsiveVal dProfile 10 25
        ]
    <|
        case maybeUserInfo of
            Nothing ->
                Element.el [ Element.centerX ] <|
                    signinElement dProfile

            Just userInfo ->
                Element.column
                    [ Element.centerX
                    , Element.spacing 10
                    ]
                    [ Element.text <| "Logged in as " ++ userInfo.email
                    , Input.button
                        [ Border.rounded 4
                        , Element.Background.color Colors.blue
                        , Element.paddingXY 20 10
                        , Font.color Colors.white
                        , Font.bold
                        , Element.centerX
                        ]
                        { onPress = Just Logout
                        , label = Element.text "Logout"
                        }
                    ]
