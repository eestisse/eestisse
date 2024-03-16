module View exposing (..)

import Browser
import Colors
import CommonView exposing (..)
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Landing.View
import List
import Route exposing (Route)
import Translation.View
import Types exposing (..)


root : FrontendModel -> Browser.Document FrontendMsg
root model =
    { title = "Eestisse"
    , body =
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
          <|
            view model
        ]
    }


view : FrontendModel -> Element FrontendMsg
view model =
    Element.column
        [ Element.width (Element.fill |> Element.maximum 700)
        , Element.centerX
        , Element.height Element.fill
        , Font.size 16
        , Element.spacing 25
        , Element.padding 10
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            ]
            [ Element.row
                [ Element.width Element.fill
                ]
                [ Element.el [ Element.width Element.fill ] <|
                    Element.none
                , Element.el [ Element.centerX ] <|
                    titleElement (model.route == Route.Landing)
                , Element.el [ Element.width Element.fill ] <|
                    if model.route == Route.Landing then
                        Element.none

                    else
                        Element.el
                            [ Element.alignRight
                            ]
                        <|
                            Input.button
                                [ Background.color <| Element.rgb 0.9 0.9 1
                                , Border.rounded 4
                                , Element.padding 4
                                ]
                                { onPress = Just <| GotoRoute Route.Landing
                                , label =
                                    Element.el
                                        [ Font.bold
                                        , Font.size 20
                                        , Font.color <| Element.rgb 0 0 1
                                        , madimiFont
                                        ]
                                        (Element.text "?")
                                }
                ]
            ]
        , case model.route of
            Route.Translate ->
                Translation.View.page model.translationPageModel

            Route.Landing ->
                Landing.View.page

            Route.BadRoute ->
                viewBadRoute
        ]


titleElement : Bool -> Element FrontendMsg
titleElement showSubtitle =
    let
        emphasizedText =
            -- Element.el [ Font.color <| Element.rgb 0.22 0.557 0.235 ] << Element.text
            Element.el [ Font.color <| Colors.darkGreen ] << Element.text

        -- Element.el [ Font.color <| Element.rgb 0.937 0.424 0 ] << Element.text
        -- Element.el [ Font.color <| Element.rgb 0.42 0.557 0.137 ] << Element.text
    in
    Element.column
        [ Element.centerX
        , madimiFont
        , Element.spacing 15
        ]
        [ Input.button
            [ Element.centerX
            , Element.paddingXY 18 8
            , Border.roundEach
                { topLeft = 25
                , bottomRight = 25
                , topRight = 3
                , bottomLeft = 3
                }
            , Background.color <| Element.rgb 0.9 0.9 1
            ]
            { onPress = Just <| GotoRoute Route.Translate
            , label =
                CommonView.coloredEestisseText [ Font.size 28, Font.italic ]
            }
        , if showSubtitle then
            Element.column
                [ Font.color <| Element.rgb 0.2 0.2 0.2
                , Font.size 20
                ]
                [ Element.row [ Element.centerX ]
                    [ Element.text "A "
                    , emphasizedText "tutoring and assitance"
                    , Element.text " tool"
                    ]
                , Element.el [ Element.centerX ] <| Element.text "for the Estonian language"
                ]

          else
            Element.none
        ]


routeLinkElement : String -> Element.Color -> Route -> Element FrontendMsg
routeLinkElement text color route =
    Input.button
        [ Element.padding 8
        , Border.rounded 5
        , Background.color color
        , Font.color <| Element.rgb 1 1 1
        , Font.size 20
        ]
        { onPress = Just <| GotoRoute route
        , label = Element.text text
        }


viewHistoryPage : Element FrontendMsg
viewHistoryPage =
    Element.text "history page"


viewBadRoute : Element FrontendMsg
viewBadRoute =
    Element.text "bad route page"
