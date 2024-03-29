module Landing.View exposing (..)

import Background.View
import Colors
import CommonTypes exposing (..)
import CommonView exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
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

        -- , futureFeaturesAndSignupElement signupState
        ]


emphasizedText : String -> Element FrontendMsg
emphasizedText =
    Element.el [ Font.bold, Font.color Colors.darkGreen ] << Element.text


primaryBox : List (Attribute msg) -> Element msg -> Element msg
primaryBox extraAttributes innerEl =
    Element.el
        ([ Element.padding 10
         , Border.rounded 30
         , Border.shadow
            { offset = ( -5, -5 )
            , size = 5
            , blur = 10
            , color = Element.rgba 0 0 0 0.3
            }

         -- , Border.color <| Colors.offWhite
         , Border.color <| Element.rgb 0.8 0.8 1
         , Border.width 10
         , Element.Background.color <| Element.rgb 0.95 0.95 1
         ]
            ++ extraAttributes
        )
        innerEl


mainExplainer : DisplayProfile -> Element FrontendMsg
mainExplainer dProfile =
    let
        italics s =
            Element.el [ Font.italic ] <| Element.text s
    in
    primaryBox
        [ Element.width Element.fill ]
    <|
        Element.column
            [ Element.spacing 20
            , Element.width Element.fill
            , Font.size <| responsiveVal dProfile 20 26
            , Font.bold
            ]
            [ CommonView.makeParagraphs
                []
                [ [ CommonView.coloredEestisseText []
                  , Element.text " (meaning \"into Estonia\") translates between English and Estonian and "
                  , emphasizedText "explains the translation"
                  , Element.text " piece by piece."
                  ]
                ]
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
                            Element.text "Enter English or Estonian text"
                , label = Input.labelHidden "Enter text"
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
        , Element.Background.color <| Element.rgb 1 1 0.9
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
                    , Element.Background.color <| Colors.blue
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
                    [ Element.padding 5
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , Border.rounded 5
                    , Border.width 1
                    , Border.color <| Element.rgb 0.7 0.7 1
                    , Element.Background.color <| Element.rgb 0.9 0.9 1
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
                    , Element.Background.color <| Element.rgb 0.9 1 0.9
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
