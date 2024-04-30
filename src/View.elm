module View exposing (..)

import Admin.View
import Background.View
import Browser
import Colors
import CommonView exposing (..)
import Config
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Landing.View
import Responsive exposing (..)
import Route exposing (Route)
import Time
import Translate.View
import Types exposing (..)
import Utils


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
                        case model.publicCredits of
                            Nothing ->
                                Element.none

                            Just publicCredits ->
                                let
                                    maybeCounterAnimationStateAndTime =
                                        model.creditsCounterAnimationState
                                            |> Maybe.map
                                                (\state ->
                                                    ( state, model.animationTime )
                                                )
                                in
                                viewPublicCredits dProfile model.showCreditCounterTooltip publicCredits maybeCounterAnimationStateAndTime

                    else
                        Element.none
                ]
            , case model.route of
                Route.Translate ->
                    Translate.View.page dProfile model.publicConsentChecked model.translationPageModel

                Route.Landing ->
                    Landing.View.page dProfile model.signupState

                Route.Admin ->
                    Admin.View.page model.maybeAdminData

                Route.Auth methodId ->
                    Element.none

                Route.BadRoute ->
                    viewBadRoute
            ]


viewPublicCredits : DisplayProfile -> Bool -> Int -> Maybe ( CreditsCounterAnimationState, Time.Posix ) -> Element FrontendMsg
viewPublicCredits dProfile showCreditCounterTooltip credits maybeCounterAnimationStateAndTime =
    let
        backgroundColor =
            let
                baseColor =
                    Element.rgba 1 1 1 0.2
            in
            case maybeCounterAnimationStateAndTime of
                Nothing ->
                    baseColor

                Just ( animationState, now ) ->
                    let
                        startColor =
                            if animationState.goingUp then
                                Element.rgba 0 1 0 0.2

                            else
                                Element.rgba 1 0 0 0.2

                        progressFloat =
                            toFloat
                                (Time.posixToMillis now
                                    - Time.posixToMillis animationState.startTime
                                )
                                / Config.counterUpdateAnimationIntervalMillis
                                |> min Config.counterUpdateAnimationIntervalMillis
                    in
                    Utils.interpolateColors
                        progressFloat
                        startColor
                        baseColor
    in
    Element.column
        [ Element.alignRight
        , Font.color Colors.darkBlue
        , Font.size <| responsiveVal dProfile 24 28
        , madimiFont
        , Element.padding <| responsiveVal dProfile 7 10
        , Element.centerY
        , Border.rounded 8
        , Border.width 1
        , Border.color <| Element.rgba 0 0 0 0.2
        , Element.Background.color backgroundColor
        , Element.pointer
        , Events.onClick <| ShowCreditCounterTooltip <| not showCreditCounterTooltip
        , Element.below <|
            if showCreditCounterTooltip then
                creditCounterTooltip dProfile credits

            else
                Element.none
        ]
        [ Element.text <| String.fromInt credits ]


creditCounterTooltip : DisplayProfile -> Int -> Element FrontendMsg
creditCounterTooltip dProfile credits =
    Element.column
        [ Element.alignRight
        , Element.width <| Element.px <| responsiveVal dProfile 280 400
        ]
        [ Element.el [ Element.height <| Element.px 10 ] Element.none
        , Element.column
            [ Element.width Element.fill
            , Element.padding 10
            , Font.size <| responsiveVal dProfile 20 24
            , Border.width 1
            , Border.rounded 10
            , Border.color <| Element.rgba 0 0 0 0.2
            , Element.Background.color <| Element.rgba 1 1 1 0.8
            , Font.color Colors.black
            , robotoFont
            , Element.spacing <| responsiveVal dProfile 15 25
            ]
            [ Element.paragraph
                [ Element.width Element.fill
                ]
                [ Element.text "There are "
                , Element.el [ madimiFont, Font.color Colors.darkBlue ] <|
                    Element.text <|
                        String.fromInt credits
                , Element.el [ Font.color Colors.darkBlue, Font.bold ] <| Element.text " credits"
                , Element.text " left for public use. A few credits are added every 5 minutes."
                ]
            , Element.paragraph
                [ Element.width Element.fill
                ]
                [ Element.text "Paid user accounts coming soon, with drastically increased usage caps." ]
            ]
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
