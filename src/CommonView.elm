module CommonView exposing (..)

import Colors
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Responsive exposing (..)
import Types exposing (..)


madimiFont : Attribute msg
madimiFont =
    Font.family
        [ Font.typeface "madimi" ]


plausibleTrackButtonClick : String -> Attribute FrontendMsg
plausibleTrackButtonClick nameForPlausible =
    let
        weirdClassName =
            "plausible-event-name=" ++ nameForPlausible
    in
    Element.htmlAttribute <|
        Html.Attributes.class weirdClassName


mainActionButton : String -> Maybe FrontendMsg -> Element FrontendMsg
mainActionButton labelText maybeMsg =
    Input.button
        [ Element.paddingXY 30 8
        , Background.color <|
            case maybeMsg of
                Just _ ->
                    Element.rgb 0 0 1

                _ ->
                    Element.rgb 0.5 0.5 0.5
        , Font.color <| Element.rgb 1 1 1
        , Font.size 26
        , Border.rounded 10
        , madimiFont
        ]
        { onPress = maybeMsg
        , label = Element.text labelText
        }


minorActionButton : String -> Maybe FrontendMsg -> Element FrontendMsg
minorActionButton labelText maybeMsg =
    Input.button
        [ Element.paddingXY 15 8
        , Border.width 1
        , Border.color <| Element.rgb 0.5 0.5 1
        , Background.color <| Element.rgb 0.9 0.9 1
        , Font.size 24
        , Font.color <| Element.rgb 0 0 0.5
        , Border.rounded 10
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
                    , Background.color <| Element.rgb 0.7 0.7 1
                    ]
                    Element.none

            else
                Element.el
                    [ Element.width <| Element.fillPortion 6
                    , Element.height <| Element.px 3
                    , Background.color <| Element.rgb 0.7 0.7 1
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


makeParagraphs : List (Attribute msg) -> List (List (Element msg)) -> Element msg
makeParagraphs extraAttributes =
    List.map
        (Element.paragraph [ Element.spacing 2 ])
        >> Element.column
            ([ Element.spacing 20
             ]
                ++ extraAttributes
            )


coloredEestisseText : List (Attribute msg) -> Element msg
coloredEestisseText extraAttributes =
    Element.row
        ([ madimiFont ] ++ extraAttributes)
        [ Element.el [ Font.color <| Colors.mainBlue ] <| Element.text "eesti"
        , Element.el [ Font.color <| Colors.teal ] <| Element.text "sse"
        ]


linkAttributes : List (Attribute msg)
linkAttributes =
    [ Font.color Colors.mainBlue
    , Font.underline
    ]


newTabLink : List (Attribute msg) -> String -> String -> Element msg
newTabLink extraAttributes url labelText =
    Element.newTabLink
        (linkAttributes ++ extraAttributes)
        { url = url
        , label = Element.text labelText
        }


htmlId : String -> Attribute msg
htmlId idStr =
    Element.htmlAttribute <| Html.Attributes.id idStr


primaryBox : List (Attribute msg) -> Element msg -> Element msg
primaryBox =
    primaryBoxCustomColors
        Colors.calmTeal
        (Element.rgb 0.95 0.95 1)


primaryBoxCustomColors : Element.Color -> Element.Color -> List (Attribute msg) -> Element msg -> Element msg
primaryBoxCustomColors borderColor backgroundColor extraAttributes innerEl =
    Element.el
        ([ Element.padding 10
         , Border.rounded 30
         , Border.shadow
            { offset = ( -5, -5 )
            , size = 5
            , blur = 10
            , color = Element.rgba 0 0 0 0.3
            }
         , Border.color borderColor
         , Border.width 10
         , Background.color backgroundColor
         ]
            ++ extraAttributes
        )
        innerEl
