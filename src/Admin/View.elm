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
    CommonView.primaryBox
        [ Element.centerX
        , Element.centerY
        , CommonView.madimiFont
        ]
    <|
        case maybeNumbers of
            Nothing ->
                Element.el [ Font.size 30 ] <| Element.text "The SUSPENSE....."

            Just labeledNumbers ->
                labeledNumbers
                    |> List.map
                        (\( label, number ) ->
                            Element.row
                                [ Element.width Element.fill
                                , Element.spacing 40
                                , Font.size 20
                                , Element.padding 3
                                ]
                                [ Element.el
                                    [ Element.alignRight
                                    , Font.color <| Colors.blue
                                    , Font.size 40
                                    ]
                                  <|
                                    Element.text <|
                                        (String.fromInt number ++ " ")
                                , Element.paragraph
                                    []
                                    [ Element.text label ]
                                ]
                        )
                    |> Element.column
                        [ Element.spacing 40
                        ]
