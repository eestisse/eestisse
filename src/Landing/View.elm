module Landing.View exposing (..)

import BackgroundAnimation
import Colors
import CommonView
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Route
import Time
import Types exposing (..)
import Utils


page : SignupState -> Element FrontendMsg
page signupState =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 25
        ]
        [ mainExplainer
        , futureFeaturesAndSignupElement signupState
        ]


emphasizedText : String -> Element FrontendMsg
emphasizedText =
    Element.el [ Font.bold, Font.color Colors.darkGreen ] << Element.text


mainExplainer : Element FrontendMsg
mainExplainer =
    let
        italics s =
            Element.el [ Font.italic ] <| Element.text s
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        , Element.padding 10
        , Border.width 1
        , Background.color <| Element.rgb 0.95 0.95 1
        , Border.color <| Element.rgb 0.8 0.8 1
        , Border.rounded 6
        ]
        [ CommonView.makeParagraphs
            []
            [ [ CommonView.coloredEestisseText []
              , Element.text " (meaning \"into Estonia\") is a tutoring and assistance tool for anyone learning the Estonian language. "
              ]
            , [ emphasizedText "Deep translation"
              , Element.text " is the central feature: Estonian is not just translated, but explained piece by piece, with the help of AI."
              ]
            ]
        , Input.button
            [ Element.paddingXY 30 8
            , Element.centerX
            , Background.color <| Element.rgb 0 0 1
            , Font.color <| Element.rgb 1 1 1
            , Font.size 22
            , Border.rounded 10
            , CommonView.madimiFont
            ]
            { onPress = Just <| GotoRoute Route.Translate
            , label = Element.text "Try out Deep Translation"
            }
        , CommonView.makeParagraphs
            []
            [ [ Element.text "As you navigate Estonia, "
              , CommonView.coloredEestisseText []
              , Element.text " can be your personal tutor, translating and explaining anything you see. "
              , Element.text "Ads, bills, news articles, and Tinder messages all work well."
              ]
            , [ Element.text "Or if you really want to learn fast, pick up an Estonian children's book! Trust me. Just tell them it's for your child... your "
              , italics "inner"
              , Element.text " child."
              ]

            -- , [ Element.text "Before long you'll find you're picking up common words and getting a feel for the grammar and the case system (of which \"eestisse\" is an example, by the way!)"
            --   ]
            ]
        ]


futureFeaturesAndSignupElement : SignupState -> Element FrontendMsg
futureFeaturesAndSignupElement signupState =
    Element.column
        [ Element.width Element.fill
        , Border.width 1
        , Background.color <| Element.rgb 1 1 0.9
        , Border.color <| Element.rgb 0.7 0.7 0
        , Border.rounded 6
        , Element.spacing 20
        , Element.padding 10
        ]
        [ CommonView.makeParagraphs
            []
            [ [ Element.text "This is just the beginning of "
              , CommonView.coloredEestisseText []
              , Element.text "! Sign up below or "
              , CommonView.newTabLink [ CommonView.plausibleTrackButtonClick "discord-link-clicked" ] "https://discord.gg/HQJMbBUmna" "join the Discord"
              , Element.text " to hear about new features as they pop: English -> Estonian deep translations, creating flash cards from translations, pictures as input, and more!"
              ]
            ]
        , signupElement signupState
        ]


signupElement : SignupState -> Element FrontendMsg
signupElement signupState =
    Element.el
        [ Element.width <| Element.px 300
        , Element.height <| Element.px 50
        , Element.centerX
        ]
    <|
        case signupState of
            Inactive ->
                Input.button
                    [ Element.paddingXY 20 10
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , Font.size 24
                    , Background.color <| Colors.blue
                    , Border.rounded 8
                    , Font.bold
                    , Font.color <| Colors.white
                    ]
                    { onPress = Just <| StartSignup
                    , label = Element.el [ Element.centerX, Element.centerY ] <| Element.text "Sign up for updates"
                    }

            Active input ->
                Element.row
                    [ Element.spacing 10
                    , Element.padding 5
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , Border.rounded 5
                    , Border.width 1
                    , Border.color <| Element.rgb 0.7 0.7 1
                    , Background.color <| Element.rgb 0.9 0.9 1
                    ]
                    [ Input.text
                        [ Element.width Element.fill
                        , Border.width 0
                        , Background.color Colors.transparent
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
                        [ Background.color <|
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
                    [ Element.padding 5
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , Border.rounded 5
                    , Border.width 1
                    , Border.color <| Element.rgb 0.7 0.7 1
                    , Background.color <| Element.rgb 0.9 0.9 1
                    ]
                <|
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        , Font.bold
                        , Font.italic
                        , Font.color <| Element.rgb 0.3 0.3 0.3
                        ]
                    <|
                        Element.text "Submitting..."

            Submitted ->
                Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Border.rounded 5
                    , Border.width 1
                    , Border.color <| Element.rgb 0.2 0.8 0.2
                    , Background.color <| Element.rgb 0.9 1 0.9
                    ]
                <|
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        , Font.bold
                        , Font.color <| Element.rgb 0 0.3 0
                        ]
                    <|
                        Element.text "Thanks! You'll hear from us soon 😊"
