module Landing.View exposing (..)

import Colors
import CommonView exposing (..)
import Config
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Responsive exposing (..)
import Route
import Types exposing (..)
import Utils


page : DisplayProfile -> Maybe FrontendUserInfo -> Element FrontendMsg
page dProfile maybeUserInfo =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 25
        ]
        [ mainExplainer dProfile
        , futureFeaturesAndSignupElement dProfile maybeUserInfo
        ]


emphasizedText : String -> Element FrontendMsg
emphasizedText =
    Element.el [ Font.bold, Font.color Colors.darkGreen ] << Element.text


mainExplainer : DisplayProfile -> Element FrontendMsg
mainExplainer dProfile =
    primaryBox
        [ Element.width Element.fill ]
    <|
        Element.column
            [ Element.spacing <| responsiveVal dProfile 20 50
            , Element.padding <| responsiveVal dProfile 5 20
            , Element.width Element.fill
            , Font.size <| responsiveVal dProfile 20 26
            ]
            [ CommonView.makeParagraphs
                [ Font.center
                , Element.centerX
                ]
                [ [ Element.text "With "
                  , CommonView.coloredEestisseText []
                  , Element.text ", every translation is a chance to learn."
                  ]
                ]
            , Element.column
                [ Element.spacing 10
                , Element.width Element.fill
                , Font.bold
                , Element.Background.color <| Element.rgba 0.9 0.9 1 0.8
                , CommonView.basicShadow
                , Element.padding <| responsiveVal dProfile 10 20
                , Border.rounded 10
                , Border.width 1
                , Border.color <| Element.rgba 0 0 0 0.1
                ]
                [ Element.el [ Font.size <| responsiveVal dProfile 22 26 ] <| Element.text "Try it out:"
                , Input.text
                    [ Border.rounded 10
                    , Element.padding <| responsiveVal dProfile 18 18
                    , Element.height <| Element.px <| responsiveVal dProfile 60 60
                    , Events.onFocus GotoTranslate_FocusAndClear
                    , Font.size <| responsiveVal dProfile 22 24
                    ]
                    { onChange = always NoOpFrontendMsg
                    , text = ""
                    , placeholder =
                        Just <|
                            Input.placeholder
                                [ Font.italic ]
                            <|
                                Element.text <|
                                    responsiveVal dProfile "Enter English or Estonian" "Enter English or Estonian text"
                    , label = Input.labelHidden "Enter text"
                    }
                ]
            , CommonView.makeParagraphs
                [ Font.center
                ]
                [ [ Element.text "AI will explain each translation piece-by-piece, providing information on case, grammar, and sentence structure." ] ]
            ]


futureFeaturesAndSignupElement : DisplayProfile -> Maybe FrontendUserInfo -> Element FrontendMsg
futureFeaturesAndSignupElement dProfile maybeUserInfo =
    primaryBox
        [ Element.width Element.fill ]
    <|
        Element.column
            [ Font.size <| responsiveVal dProfile 20 26
            , Element.spacing <| responsiveVal dProfile 20 30
            , Element.padding <| responsiveVal dProfile 5 20
            ]
            [ CommonView.makeParagraphs
                [ Font.center
                ]
                [ [ Element.text "Eestisse's goal is to make learning Estonian "
                  , Element.el [ Font.bold ] <| Element.text "fun and relevant to your life"
                  , Element.text ", and this is just the beginning!"
                  ]
                ]
            , case maybeUserInfo of
                Nothing ->
                    makeParagraphs
                        [ Font.center ]
                        [ [ actionLink "Sign up" <| GotoRouteAndAnimate Route.Account
                          , Element.text " or "
                          , joinDiscordLink "join the Discord"
                          , Element.text " to hear about when more features drop."
                          ]
                        ]

                Just _ ->
                    makeParagraphs
                        [ Font.center, Element.width Element.fill ]
                        [ [ joinDiscordLink "join the Discord"
                          , Element.text " if you have questions or comments."
                          ]
                        ]
            ]
