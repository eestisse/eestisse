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


futureFeaturesAndSignupElement : DisplayProfile -> SignupState -> Element FrontendMsg
futureFeaturesAndSignupElement dProfile signupState =
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

                Active signupForm ->
                    viewSignupForm dProfile signupForm

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


viewSignupForm : DisplayProfile -> SignupFormModel -> Element FrontendMsg
viewSignupForm dProfile formModel =
    Element.column
        [ Element.height Element.fill
        , Element.padding 10
        , Element.centerX
        , Border.width 1
        , Border.color <| Element.rgba 0 0 0 0.1
        , Border.rounded 5
        , Element.Background.color Colors.lightBlue
        , Element.spacing 10
        , CommonView.basicShadow
        ]
        [ Input.text
            [ Element.width Element.fill
            , Element.padding 10
            , Border.rounded 5
            , Border.width 1
            , Border.color <| Element.rgb 0.7 0.7 1
            , Element.Background.color <| Element.rgb 0.98 0.98 1
            , Utils.onEnter <|
                if isSignupFormReadyToSubmit formModel then
                    SubmitSignupClicked formModel

                else
                    NoOpFrontendMsg
            , CommonView.htmlId "email-input"
            ]
            { onChange = \text -> SignupFormChanged <| { formModel | emailInput = text }
            , text = formModel.emailInput
            , placeholder = Just <| Input.placeholder [ Font.color <| Element.rgb 0.5 0.5 0.5 ] <| Element.text "you@somewhere.neat"
            , label = Input.labelHidden "email input"
            }
        , consentOptions dProfile formModel
        , Input.button
            [ Element.Background.color <|
                if isSignupFormReadyToSubmit formModel then
                    Colors.blue

                else
                    Colors.gray
            , Element.width <| Element.px 32
            , Element.height <| Element.px 32
            , Border.rounded 4
            ]
            { onPress =
                if isSignupFormReadyToSubmit formModel then
                    Just <| SubmitSignupClicked formModel

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


consentOptions : DisplayProfile -> SignupFormModel -> Element FrontendMsg
consentOptions dProfile formModel =
    Element.column
        [ Element.spacing 10
        ]
        [ Element.el [ Font.size <| responsiveVal dProfile 18 20 ] <|
            Element.text "I am interested in..."
        , consentCheckbox dProfile Config.newFeaturesConsentWording formModel.newFeaturesConsentChecked (\b -> { formModel | newFeaturesConsentChecked = b })
        , consentCheckbox dProfile Config.userInterviewsConsentWording formModel.userInterviewsConsentChecked (\b -> { formModel | userInterviewsConsentChecked = b })
        ]


consentCheckbox : DisplayProfile -> String -> Bool -> (Bool -> SignupFormModel) -> Element FrontendMsg
consentCheckbox dProfile text checked formUpdater =
    Input.checkbox
        []
        { onChange = formUpdater >> SignupFormChanged
        , icon = Input.defaultCheckbox
        , checked = checked
        , label =
            Input.labelRight
                [ Element.paddingEach
                    { left = 8
                    , right = 0
                    , bottom = 0
                    , top = 0
                    }
                , Font.size <| responsiveVal dProfile 16 18
                , Element.width Element.fill
                ]
            <|
                Element.paragraph [ Element.spacing 2 ] [ Element.text text ]
        }


isSignupFormReadyToSubmit : SignupFormModel -> Bool
isSignupFormReadyToSubmit signupForm =
    Utils.isValidEmail signupForm.emailInput
        && (signupForm.newFeaturesConsentChecked || signupForm.userInterviewsConsentChecked)
