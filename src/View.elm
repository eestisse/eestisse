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
            [ Element.width (Element.fill |> Element.maximum 700)
            , Element.centerX
            , Element.height Element.fill
            , Font.size 16
            , Element.spacing <| responsiveVal dProfile 25 60
            , Element.padding 10
            ]
            [ Element.row
                [ Element.width Element.fill
                ]
                [ Element.el [ Element.width Element.fill ] <|
                    Element.none
                , Element.el [ Element.centerX ] <|
                    titleElement dProfile (model.route == Route.Landing)
                , Element.el [ Element.width Element.fill ] <|
                    Element.none
                ]
            , case model.route of
                Route.Translate ->
                    Translate.View.page model.translationPageModel

                Route.Landing ->
                    Landing.View.page dProfile model.signupState

                Route.Admin ->
                    Admin.View.page model.maybeImportantNumber

                Route.BadRoute ->
                    viewBadRoute
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
            , Element.Background.color <| Element.rgb 0.9 0.9 1
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
                    [ Element.text "A "
                    , emphasizedText "tutor in your pocket"
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
