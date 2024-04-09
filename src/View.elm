module View exposing (..)

import Admin.View
import Background.View
import Browser
import Colors
import CommonView exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Landing.View
import Responsive exposing (..)
import Route exposing (Route)
import Translate.View
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
            , robotoFont
            ]
          <|
            case model.dProfile of
                Just dProfile ->
                    view dProfile model

                Nothing ->
                    Element.none
        ]
    }


view : DisplayProfile -> FrontendModel -> Element FrontendMsg
view dProfile model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , case model.backgroundModel of
            Just backgroundModel ->
                Element.behindContent <| Background.View.view dProfile model.animationTime backgroundModel

            Nothing ->
                Element.Background.color <| Colors.vibrantTeal
        ]
    <|
        Element.column
            [ Element.width <| responsiveVal dProfile Element.fill (Element.fill |> Element.maximum 900)
            , Element.centerX
            , Element.height Element.fill
            , Font.size 16
            , Element.spacing <| responsiveVal dProfile 25 40
            , Element.padding 10
            ]
            [ Element.row
                [ Element.width Element.fill
                ]
                [ Element.el [ Element.width Element.fill ] <|
                    if model.route == Route.Translate then
                        Input.button
                            [ Element.padding 10
                            , Border.rounded 10
                            , Element.Background.color <| Colors.lightBlue
                            ]
                            { onPress = Just <| GotoRoute Route.Landing
                            , label =
                                Element.image
                                    [ Element.height <| Element.px <| responsiveVal dProfile 20 30
                                    ]
                                    { src = "left-arrow-black.png"
                                    , description = "back"
                                    }
                            }

                    else
                        Element.none
                , Element.el [ Element.centerX ] <|
                    titleElement dProfile (model.route == Route.Landing)
                , Element.el [ Element.width Element.fill ] <|
                    if model.route == Route.Translate then
                        Maybe.map (viewPublicCredits dProfile) model.publicCredits
                            |> Maybe.withDefault Element.none

                    else
                        Element.none
                ]
            , case model.route of
                Route.Translate ->
                    Translate.View.page dProfile model.translationPageModel

                Route.Landing ->
                    Landing.View.page dProfile model.signupState

                Route.Admin ->
                    Admin.View.page model.maybeImportantNumbers

                Route.BadRoute ->
                    viewBadRoute
            ]


viewPublicCredits : DisplayProfile -> Int -> Element FrontendMsg
viewPublicCredits dProfile credits =
    Element.column
        [ Element.alignRight
        , Font.color <| Element.rgb 0 0.2 1
        , Font.size <| responsiveVal dProfile 24 28
        , madimiFont
        , Element.padding <| responsiveVal dProfile 7 10
        , Element.centerY
        , Border.rounded 8
        , Border.width 1
        , Border.color <| Element.rgba 0 0 0 0.2
        , Element.Background.color <| Element.rgba 1 1 1 0.2
        ]
        [ Element.text <| String.fromInt credits
        ]


titleElement : DisplayProfile -> Bool -> Element FrontendMsg
titleElement dProfile showSubtitle =
    let
        emphasizedText =
            Element.el [ Font.color <| Colors.darkGreen ] << Element.text
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
            , Element.Background.color Colors.lightBlue
            ]
            { onPress = Just <| GotoRoute Route.Landing
            , label =
                CommonView.coloredEestisseText
                    [ Font.size <| responsiveVal dProfile 28 42
                    , Font.italic
                    ]
            }
        , if showSubtitle then
            Element.column
                [ Font.color <| Element.rgb 0.2 0.2 0.2
                , Font.size <| responsiveVal dProfile 24 36
                ]
                [ Element.row [ Element.centerX ]
                    [ emphasizedText "An Estonian tutor in your pocket"
                    ]
                ]

          else
            Element.none
        ]


routeLinkElement : String -> Element.Color -> Route -> Element FrontendMsg
routeLinkElement text color route =
    Input.button
        [ Element.padding 8
        , Border.rounded 5
        , Element.Background.color color
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
