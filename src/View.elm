module View exposing (..)

import About.View
import Browser
import CommonView exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
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
        , Element.Font.size 16
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
                    if model.route == Route.Translate then
                        Element.none

                    else
                        Element.el [ Element.alignLeft ] <|
                            routeLinkElement "translate" (Element.rgb 0 0 1) Route.Translate
                , Element.el [ Element.centerX ]
                    titleElement
                , Element.el [ Element.width Element.fill ] <|
                    if model.route == Route.About then
                        Element.none

                    else
                        Element.el [ Element.alignRight ] <|
                            routeLinkElement "?" (Element.rgb 0 0 1) Route.About
                ]
            , if model.showExplainerSubtitle then
                explainerSubtitleElement

              else
                Element.none
            ]
        , case model.route of
            Route.Translate ->
                Translation.View.page model.translationPageModel

            Route.About ->
                About.View.page

            Route.BadRoute ->
                viewBadRoute
        ]


titleElement : Element FrontendMsg
titleElement =
    Element.row
        [ Element.centerX
        , Element.Font.size 28
        , Element.Font.italic
        , madimiFont
        ]
        [ Element.el [ Element.Font.color <| Element.rgb 0.2 0.2 1 ] <| Element.text "eesti"
        , Element.el [ Element.Font.color <| Element.rgb 0 0.5 0.8 ] <| Element.text "sse"
        ]


routeLinkElement : String -> Element.Color -> Route -> Element FrontendMsg
routeLinkElement text color route =
    Element.Input.button
        [ Element.padding 8
        , Element.Border.rounded 5
        , Element.Background.color color
        , Element.Font.color <| Element.rgb 1 1 1
        , Element.Font.size 20
        ]
        { onPress = Just <| GotoRoute route
        , label = Element.text text
        }


explainerSubtitleElement : Element FrontendMsg
explainerSubtitleElement =
    [ [ Element.text "Eestisse helps you learn as you translate." ]
    , [ Element.text "It really shines with longer sentences!" ]
    ]
        |> List.map
            (Element.paragraph
                [ Element.Font.center
                , Element.Font.italic
                , Element.spacing 2
                ]
            )
        |> Element.column
            [ Element.centerX
            , Element.padding 5
            , Element.Border.width 1
            , Element.Border.color <| Element.rgb 0.8 0.8 1
            , Element.Background.color <| Element.rgb 0.9 0.9 1
            , Element.Border.rounded 6
            , Element.spacing 10
            ]


viewHistoryPage : Element FrontendMsg
viewHistoryPage =
    Element.text "history page"


viewBadRoute : Element FrontendMsg
viewBadRoute =
    Element.text "bad route page"
