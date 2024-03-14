module CommonView exposing (..)

import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Types exposing (..)


madimiFont : Attribute FrontendMsg
madimiFont =
    Element.Font.family
        [ Element.Font.typeface "madimi" ]


mainActionButton : String -> Maybe FrontendMsg -> Element FrontendMsg
mainActionButton labelText maybeMsg =
    Element.Input.button
        [ Element.paddingXY 30 8
        , Element.Background.color <|
            case maybeMsg of
                Just _ ->
                    Element.rgb 0 0 1

                _ ->
                    Element.rgb 0.5 0.5 0.5
        , Element.Font.color <| Element.rgb 1 1 1
        , Element.Font.size 26
        , Element.Border.rounded 10
        , madimiFont
        ]
        { onPress = maybeMsg
        , label = Element.text labelText
        }


minorActionButton : String -> Maybe FrontendMsg -> Element FrontendMsg
minorActionButton labelText maybeMsg =
    Element.Input.button
        [ Element.paddingXY 15 8
        , Element.Border.width 1
        , Element.Border.color <| Element.rgb 0.5 0.5 1
        , Element.Background.color <| Element.rgb 0.9 0.9 1
        , Element.Font.size 24
        , Element.Font.color <| Element.rgb 0 0 0.5
        , Element.Border.rounded 10
        ]
        { onPress = maybeMsg
        , label = Element.el [ Element.centerX, Element.centerY ] <| Element.text labelText
        }


hbreakElement : Element FrontendMsg
hbreakElement =
    breakElement False


vbreakElement : Element FrontendMsg
vbreakElement =
    breakElement True


breakElement : Bool -> Element FrontendMsg
breakElement isVertical =
    let
        outerEl =
            if isVertical then
                Element.el [ Element.height <| Element.fillPortion 1 ] Element.none

            else
                Element.el [ Element.width <| Element.fillPortion 1 ] Element.none

        innerEl =
            if isVertical then
                Element.el
                    [ Element.height <| Element.fillPortion 6
                    , Element.width <| Element.px 2
                    , Element.Background.color <| Element.rgb 0.7 0.7 1
                    ]
                    Element.none

            else
                Element.el
                    [ Element.width <| Element.fillPortion 6
                    , Element.height <| Element.px 3
                    , Element.Background.color <| Element.rgb 0.7 0.7 1
                    ]
                <|
                    Element.none
    in
    (if isVertical then
        Element.column
            [ Element.height Element.fill
            , Element.paddingXY 5 0
            ]

     else
        Element.row
            [ Element.width Element.fill
            , Element.paddingXY 0 7
            ]
    )
        [ outerEl
        , innerEl
        , outerEl
        ]
