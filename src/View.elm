module View exposing (..)

import Account.View
import Admin.View
import Background.View
import Browse.View
import Browser
import Colors
import CommonView exposing (..)
import Config
import Dict exposing (Dict)
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
import Subscribe.View
import Time
import Translation.Types exposing (..)
import Translation.View
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
            [ Element.column
                [ Element.width Element.fill
                , Element.spacing <| responsiveVal dProfile 5 15
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
                                { onPress = Just <| GotoRouteAndAnimate Route.Landing
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
                        titleElement dProfile
                    , Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        ]
                      <|
                        if model.route /= Route.Account && model.route /= Route.Subscribe && not (Route.shouldRedirect model.route) then
                            Element.el
                                [ Element.alignRight
                                , Element.alignTop
                                , Element.padding 10
                                ]
                            <|
                                signInOrAccountButton dProfile model.maybeAuthedUserInfo

                        else
                            Element.none
                    ]
                , if model.route == Route.Landing then
                    Element.el [ Element.centerX ] <| subtitleElement dProfile

                  else
                    Element.none
                ]
            , case model.route of
                Route.Translate ->
                    Translation.View.viewDoTranslatePage dProfile model.maybePublicCreditsInfo model.time_updatePerSecond model.maybeAuthedUserInfo model.doTranslateModel model.publicConsentChecked model.loadingAnimationCounter

                Route.Landing ->
                    Landing.View.page dProfile model.maybeAuthedUserInfo

                Route.Admin ->
                    Admin.View.page model.maybeAdminData

                Route.AuthCallback _ ->
                    Element.el [ Element.centerX ] <| Element.text "User authenticated. Redirecting..."

                Route.Account ->
                    Account.View.page dProfile model.maybeAuthedUserInfo

                Route.Subscribe ->
                    Subscribe.View.page dProfile model

                Route.Browse ->
                    Browse.View.page dProfile model.cachedTranslationRecords

                Route.View id ->
                    case getTranslationRecord id model of
                        Just translationRecord ->
                            Translation.View.viewTranslationPage dProfile translationRecord model.viewTranslationModel

                        Nothing ->
                            Translation.View.viewLoadingTranslationPage dProfile

                Route.BadRoute ->
                    viewBadRoute
            ]


signInOrAccountButton : DisplayProfile -> Maybe FrontendUserInfo -> Element FrontendMsg
signInOrAccountButton dProfile maybeUserInfo =
    case maybeUserInfo of
        Nothing ->
            Input.button
                [ responsiveVal dProfile
                    (Element.paddingXY 18 8)
                    (Element.paddingXY 30 10)
                , Border.rounded 4
                , Element.Background.color Colors.blue
                , Font.color Colors.white
                , Font.bold
                , Font.size <| responsiveVal dProfile 14 16
                ]
                { onPress = Just <| GotoRouteAndAnimate <| Route.Account
                , label = Element.text "Sign In"
                }

        Just userInfo ->
            Input.button
                [ Border.rounded 200 -- easy way to make a circle! :D
                , Element.width <| Element.px 40
                , Element.height <| Element.px 40
                , Element.Background.color Colors.offWhite
                ]
                { onPress = Just <| GotoRouteAndAnimate <| Route.Account
                , label =
                    let
                        userCharString =
                            userInfo.email |> String.toUpper |> String.toList |> List.head |> Maybe.withDefault '?' |> String.fromChar
                    in
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        , Font.bold
                        , madimiFont
                        , Font.color Colors.blue
                        , Font.size 18
                        ]
                    <|
                        Element.text userCharString
                }


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
                , Element.text " left for public use. Credits are added slowly over time and cap out at 100."
                ]
            , Element.paragraph
                [ Element.width Element.fill
                ]
                [ Element.text "Paid user accounts coming soon, with drastically increased usage caps." ]
            ]
        ]


emphasizedText : String -> Element FrontendMsg
emphasizedText =
    Element.el [ Font.color <| Colors.darkGreen ] << Element.text


titleElement : DisplayProfile -> Element FrontendMsg
titleElement dProfile =
    Input.button
        [ Element.centerX
        , madimiFont
        , Element.paddingXY 18 8
        , Border.roundEach
            { topLeft = 25
            , bottomRight = 25
            , topRight = 3
            , bottomLeft = 3
            }
        , Element.Background.color Colors.lightBlue
        ]
        { onPress = Just <| GotoRouteAndAnimate Route.Landing
        , label =
            CommonView.coloredEestisseText
                [ Font.size <| responsiveVal dProfile 28 42
                , Font.italic
                ]
        }


subtitleElement : DisplayProfile -> Element FrontendMsg
subtitleElement dProfile =
    Element.column
        [ Font.color <| Element.rgb 0.2 0.2 0.2
        , Font.size <| responsiveVal dProfile 24 36
        , madimiFont
        ]
        [ Element.row [ Element.centerX ]
            [ emphasizedText "An Estonian tutor in your pocket"
            ]
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
        { onPress = Just <| GotoRouteAndAnimate route
        , label = Element.text text
        }


viewHistoryPage : Element FrontendMsg
viewHistoryPage =
    Element.text "history page"


viewBadRoute : Element FrontendMsg
viewBadRoute =
    Element.text "bad route page"
