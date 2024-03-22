module Admin.View exposing (..)

import Colors
import CommonView
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Types exposing (..)


page : Maybe Int -> Element FrontendMsg
page maybeHowMany =
    Element.el
        [ Element.centerX
        , Element.centerY
        , CommonView.madimiFont
        ]
    <|
        case maybeHowMany of
            Nothing ->
                Input.button
                    [ Element.Background.color Colors.blue
                    , Border.rounded 5
                    , Font.color Colors.white
                    , Font.bold
                    , Font.size 36
                    , Element.paddingXY 40 20
                    ]
                    { onPress = Just <| FetchImportantNumber
                    , label = Element.text "KUI PALJU"
                    }

            Just howMany ->
                Element.el
                    [ Font.size 80
                    ]
                <|
                    Element.text <|
                        String.fromInt howMany
