module Landing.View exposing (..)

import Background.View
import Colors
import CommonView exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Responsive exposing (..)
import Route
import Time
import Types exposing (..)
import Utils


page : DisplayProfile -> SignupState -> Element FrontendMsg
page dProfile signupState =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 25
        ]
        [ mainExplainer dProfile
        , futureFeaturesAndSignupElement dProfile signupState
        ]


emphasizedText : String -> Element FrontendMsg
emphasizedText =
    Element.el [ Font.bold, Font.color Colors.darkGreen ] << Element.text


mainExplainer : DisplayProfile -> Element FrontendMsg
mainExplainer dProfile =
    primaryBox
        Colors.calmTeal
        (Element.rgb 0.95 0.95 1)
        [ Element.width Element.fill ]
    <|
        Element.column
            [ Element.spacing <| responsiveVal dProfile 20 50
            , Element.padding <| responsiveVal dProfile 5 20
            , Element.width Element.fill
            , Font.size <| responsiveVal dProfile 20 30
            ]
            [ CommonView.makeParagraphs
                [ Font.center
                ]
                [ [ Element.text "With "
                  , CommonView.coloredEestisseText []
                  , Element.text ", every translation becomes a learning opportunity."
                  ]
                ]
            , Element.column
                [ Element.spacing 10
                , Element.width Element.fill
                , Font.bold
                , Element.Background.color <| Element.rgba 0.9 0.9 1 0.8
                , Border.shadow
                    { offset = ( -3, 3 )
                    , size = 1
                    , blur = 5
                    , color = Element.rgb 0.8 0.8 0.8
                    }
                , Element.padding <| responsiveVal dProfile 10 20
                , Border.rounded 10
                , Border.width 1
                , Border.color <| Element.rgba 0 0 0 0.1
                ]
                [ Element.el [ Font.size <| responsiveVal dProfile 22 30 ] <| Element.text "Try it out:"
                , Input.text
                    [ Border.rounded 10
                    , Element.padding <| responsiveVal dProfile 18 20
                    , Element.height <| Element.px <| responsiveVal dProfile 60 70
                    , Events.onFocus GotoTranslateAndFocus
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


futureFeaturesAndSignupElement : DisplayProfile -> SignupState -> Element FrontendMsg
futureFeaturesAndSignupElement dProfile signupState =
    primaryBox Colors.calmTeal
        (Element.rgb 0.95 0.95 1)
        [ Element.width Element.fill ]
    <|
        Element.column
            [ Font.size <| responsiveVal dProfile 20 30
            , Element.spacing <| responsiveVal dProfile 20 30
            , Element.padding <| responsiveVal dProfile 5 20
            ]
            [ CommonView.makeParagraphs
                [ Font.center
                ]
                [ [ Element.text "Eestisse's goal is to make learning Estonian "
                  , Element.el [ Font.bold ] <| Element.text "fun, easy, and effective"
                  , Element.text ", and this is just the beginning!"
                  ]
                ]
            , case signupState of
                Inactive ->
                    CommonView.makeParagraphs
                        [ Font.center ]
                        [ [ Input.button
                                CommonView.linkAttributes
                                { onPress = Just <| StartSignup
                                , label = Element.text "Sign up"
                                }
                          , Element.text " or "
                          , CommonView.newTabLink [ CommonView.plausibleTrackButtonClick "discord-link-clicked" ] "https://discord.gg/HQJMbBUmna" "join the Discord"
                          , Element.text " to hear about when more features drop."
                          ]
                        ]

                Active input ->
                    Element.row
                        [ Element.spacing 10
                        , Element.padding 5
                        , Element.width Element.fill
                        , Element.height Element.fill
                        , Border.rounded 5
                        , Border.width 1
                        , Border.color <| Element.rgb 0.7 0.7 1
                        , Element.Background.color <| Element.rgb 0.9 0.9 1
                        ]
                        [ Input.text
                            [ Element.width Element.fill
                            , Border.width 0
                            , Element.Background.color Colors.transparent
                            , Utils.onEnter <|
                                if Utils.isValidEmail input then
                                    SubmitSignup input

                                else
                                    NoOpFrontendMsg
                            , CommonView.htmlId "email-input"
                            ]
                            { onChange = SignupTextChanged
                            , text = input
                            , placeholder = Just <| Input.placeholder [ Font.color <| Element.rgb 0.5 0.5 0.5 ] <| Element.text "you@somewhere.neat"
                            , label = Input.labelHidden "email input"
                            }
                        , Input.button
                            [ Element.Background.color <|
                                if Utils.isValidEmail input then
                                    Colors.blue

                                else
                                    Colors.gray
                            , Element.width <| Element.px 32
                            , Element.height <| Element.px 32
                            , Border.rounded 4
                            ]
                            { onPress =
                                if Utils.isValidEmail input then
                                    Just <| SubmitSignup input

                                else
                                    Nothing
                            , label =
                                Element.image
                                    [ Element.centerX
                                    , Element.centerY
                                    , Element.width <| Element.px 26
                                    ]
                                    { src = "/enter-arrow-white.png"
                                    , description = "submit email"
                                    }
                            }
                        ]

                Submitting ->
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        , Font.italic
                        , Font.color <| Element.rgb 0.5 0.5 0.5
                        ]
                    <|
                        Element.text "Submitting..."

                Submitted ->
                    CommonView.makeParagraphs
                        [ Font.center ]
                        [ [ Element.text "Thanks for signing up! You can also "
                          , CommonView.newTabLink [ CommonView.plausibleTrackButtonClick "discord-link-clicked" ] "https://discord.gg/HQJMbBUmna" "join the Discord"
                          , Element.text " if you have questions or comments."
                          ]
                        ]
            ]
