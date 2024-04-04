module Admin.View exposing (..)

import Colors
import CommonView
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Types exposing (..)


page : Maybe (List ( String, Int )) -> Element FrontendMsg
page maybeNumbers =
    Element.el
        [ Element.centerX
        , Element.centerY
        , CommonView.madimiFont
        ]
    <|
        case maybeNumbers of
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

            Just labeledNumbers ->
                labeledNumbers
                    |> List.map
                        (\( label, number ) ->
                            Element.row [ Element.width Element.fill, Element.spacing 20 ]
                                [ Element.text label
                                , Element.el [ Element.alignRight ] <| Element.text <| String.fromInt number
                                ]
                        )
                    |> Element.column [ Element.spacing 20 ]
