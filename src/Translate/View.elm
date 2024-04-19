module Translate.View exposing (..)

import Colors
import CommonView exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import List.Extra
import Responsive exposing (..)
import Types exposing (..)
import Utils


page : DisplayProfile -> Maybe FrontendUserInfo -> TranslationPageModel -> Element FrontendMsg
page dProfile maybeAuthedUserInfo translationPageModel =
    case translationPageModel of
        InputtingText translationInputModel ->
            viewTranslationPageInput dProfile maybeAuthedUserInfo translationInputModel

        RequestSent requestState ->
            viewTranslationPageRequestState dProfile requestState


viewTranslationPageInput : DisplayProfile -> Maybe FrontendUserInfo -> TranslationInputModel -> Element FrontendMsg
viewTranslationPageInput dProfile maybeAuthedUserInfo translationInputModel =
    let
        submitMsgIfEnabled =
            if translationInputModel.input /= "" && translationInputModel.publicConsentChecked then
                Just <| SubmitText translationInputModel.publicConsentChecked translationInputModel.input

            else
                Nothing
    in
    CommonView.primaryBoxCustomColors
        Colors.calmTeal
        Colors.calmTeal
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 0
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 20
            ]
            [ CommonView.scrollbarYEl [] <|
                Input.multiline
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Border.rounded 20
                    , Element.padding 10
                    , Border.width 0
                    , Font.size translateTextSize
                    , CommonView.htmlId "translate-input"
                    ]
                    { onChange = \t -> TranslationInputModelChanged { translationInputModel | input = t }
                    , text = translationInputModel.input
                    , placeholder =
                        Just <|
                            Input.placeholder
                                [ Font.italic
                                ]
                            <|
                                Element.column
                                    [ Element.spacing 10
                                    ]
                                    [ Element.text "Enter English or Estonian text here!"
                                    , Element.el [ Element.height <| Element.px 5 ] <| Element.none
                                    , Element.text "• minu nimi on Bob"
                                    , Element.text "• where can I find cooking oil?"
                                    , Element.text "• põnev!"
                                    , Element.text "• How much wood would a woodchuck chuck if a woodchuck could chuck wood?"
                                    ]
                    , label = Input.labelHidden "Enter text"
                    , spellcheck = False
                    }
            , Element.column
                [ Element.width Element.fill
                , Element.alignBottom
                , Element.width Element.fill
                , Element.spacing 10
                ]
                [ Input.checkbox
                    [ Element.centerX
                    , Element.width Element.shrink
                    ]
                    { onChange = \f -> TranslationInputModelChanged { translationInputModel | publicConsentChecked = f }
                    , icon = Input.defaultCheckbox
                    , checked = translationInputModel.publicConsentChecked
                    , label =
                        Input.labelRight
                            [ Font.size <| responsiveVal dProfile 18 20
                            , Font.color <| Element.rgb 0.3 0.3 0.3
                            ]
                        <|
                            Element.text "Share translation publicly"
                    }
                , if translationInputModel.publicConsentChecked || maybeFrontendUserHasActiveMembership maybeAuthedUserInfo then
                    Element.el [ Element.centerX ] <| translateButton submitMsgIfEnabled

                  else
                    Element.text "hold up there bucko"
                ]
            ]


viewTranslationPageRequestState : DisplayProfile -> RequestState -> Element FrontendMsg
viewTranslationPageRequestState dProfile requestState =
    primaryBox
        [ Element.width Element.fill ]
    <|
        Element.column
            [ Element.spacing 15
            , Element.padding 10
            , Element.width Element.fill
            , Element.height Element.fill
            , Font.size <| responsiveVal dProfile 18 24
            ]
        <|
            case requestState of
                Waiting inputText animationCounter ->
                    [ Element.el [ Element.centerX ] <| translationInputTextElement inputText
                    , Element.row
                        [ Element.width Element.fill ]
                        [ hbreakElement
                        , loadingEmojiAnimation animationCounter
                        , hbreakElement
                        ]
                    , abortButton inputText
                    ]

                RequestComplete completedRequest ->
                    let
                        editOrNewButtonsRow =
                            Element.row
                                [ Element.alignBottom
                                , Element.width Element.fill
                                , Element.spacing 20
                                ]
                                [ Element.el [ Element.alignLeft ] <| modifyTextButton completedRequest.inputText
                                , Element.el [ Element.alignRight ] <| newTranslationButton
                                ]
                    in
                    case completedRequest.translationResult of
                        Err gptAssistError ->
                            [ translationInputTextElement completedRequest.inputText
                            , hbreakElement
                            , viewGptAssistError gptAssistError
                            , editOrNewButtonsRow
                            ]

                        Ok translation ->
                            [ translationInputButtonsElement translation.breakdown completedRequest.maybeSelectedBreakdownPart
                            , hbreakElement
                            , englishTextElement <|
                                case translation.translatedTo of
                                    English ->
                                        translation.translation

                                    Estonian ->
                                        completedRequest.inputText
                            , Element.el [ Element.height <| Element.px 8 ] <| Element.none
                            , Element.el [ Element.width Element.fill ] <|
                                -- wrapping in an extra el due to elm-ui bug https://github.com/mdgriffith/elm-ui/issues/270
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height (Element.shrink |> Element.minimum 200)
                                    ]
                                <|
                                    Element.el
                                        [ Element.centerY
                                        , Element.centerX
                                        ]
                                    <|
                                        case completedRequest.maybeSelectedBreakdownPart of
                                            Just selectedBreakdownPart ->
                                                selectedExplanationElement dProfile selectedBreakdownPart

                                            Nothing ->
                                                clickOnPartsHint dProfile
                            , editOrNewButtonsRow
                            ]


abortButton : String -> Element FrontendMsg
abortButton inputText =
    minorActionButton "Abort and Edit" (Just <| EditTranslation inputText)


translateTextSize : Int
translateTextSize =
    24


translatedTextColor : Element.Color
translatedTextColor =
    Element.rgb 0.2 0.2 0.2


translatedTextStyles : List (Attribute FrontendMsg)
translatedTextStyles =
    [ Font.size translateTextSize
    , Element.width Element.fill
    , Font.color translatedTextColor
    , Font.italic
    ]


translationInputTextElement : String -> Element FrontendMsg
translationInputTextElement inputText =
    Element.el [] <|
        Element.paragraph
            [ Font.size translateTextSize
            , Element.width Element.fill
            ]
            [ Element.text inputText ]


translationInputButtonsElement : Breakdown -> Maybe BreakdownPart -> Element FrontendMsg
translationInputButtonsElement breakdown maybeSelectedBreakdownPart =
    let
        partIsSelected estonian =
            case maybeSelectedBreakdownPart of
                Just selectedBreakdownPart ->
                    estonian == selectedBreakdownPart.estonian

                Nothing ->
                    False
    in
    Element.paragraph
        [ Font.size translateTextSize
        , Element.width Element.fill
        ]
        (breakdown
            |> List.map
                (\breakdownPart ->
                    selectPartButton (partIsSelected breakdownPart.estonian) breakdownPart
                )
            |> List.intersperse (Element.text " ")
        )


selectPartButton : Bool -> BreakdownPart -> Element FrontendMsg
selectPartButton partIsSelected breakdownPart =
    Input.button
        ([ Element.padding 2
         , Border.width 1
         , Border.rounded 5
         , Border.color <| Element.rgb 0.5 0.5 1
         ]
            ++ (if partIsSelected then
                    [ Element.Background.color <| Element.rgb 0.3 0.3 1
                    , Font.color <| Element.rgb 1 1 1
                    ]

                else
                    [ Element.Background.color <| Element.rgb 0.9 0.95 1 ]
               )
        )
        { onPress = Just <| ShowExplanation breakdownPart
        , label = Element.text breakdownPart.estonian
        }


englishTextElement : String -> Element FrontendMsg
englishTextElement translatedText =
    Element.el [] <|
        Element.paragraph
            translatedTextStyles
            [ Element.text translatedText ]


loadingEmojiAnimation : Int -> Element FrontendMsg
loadingEmojiAnimation animationCounter =
    let
        emojiRow =
            "🤔🧐😤💭"
                |> String.toList
                |> List.map String.fromChar
                |> List.Extra.getAt animationCounter
                |> Maybe.map Element.text
                |> Maybe.withDefault Element.none
    in
    Element.el
        [ Font.size 24
        , Font.color <| Element.rgb 0.5 0.5 0.5
        , Element.centerX
        ]
        emojiRow


clickOnPartsHint : DisplayProfile -> Element FrontendMsg
clickOnPartsHint dProfile =
    Element.row
        [ Element.spacing 10
        , Element.Background.color <| Element.rgb 0.8 1 0.8
        , Border.width 1
        , Border.rounded 10
        , Border.color <| Element.rgb 0.6 0.8 0.6
        , Element.padding 8
        ]
        [ Element.image
            [ Element.width <| Element.px <| responsiveVal dProfile 26 32
            ]
            { src = "/info-icon.png"
            , description = "submit email"
            }
        , Element.paragraph
            [ Font.center
            , Element.centerY
            , Font.size <| responsiveVal dProfile 20 26
            , Font.bold
            ]
            [ Element.text "Tap parts of the Estonian text to learn more!" ]
        ]


selectedExplanationElement : DisplayProfile -> BreakdownPart -> Element FrontendMsg
selectedExplanationElement dProfile breakdownPart =
    Element.column
        [ Element.spacing 5
        , Element.Background.color <| Element.rgb 0.8 1 0.8
        , Border.width 1
        , Border.rounded 10
        , Border.color <| Element.rgb 0.6 0.8 0.6
        , Element.padding 8
        , Element.centerX
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 5
            ]
            [ Element.el
                [ Element.width Element.fill
                ]
              <|
                Element.paragraph
                    [ Element.alignRight
                    , Element.width Element.shrink
                    , Font.bold
                    , Font.size <| responsiveVal dProfile 20 26
                    ]
                    [ Element.text breakdownPart.estonian ]
            , vbreakElement
            , Element.el
                [ Element.width Element.fill
                ]
              <|
                Element.paragraph
                    [ Element.alignLeft
                    , Font.italic
                    , Font.color translatedTextColor
                    , Font.size <| responsiveVal dProfile 20 26
                    ]
                    [ Element.text breakdownPart.englishTranslation ]
            ]
        , breakdownPart.maybeExplanation
            |> Maybe.map
                (\explanationText ->
                    Element.paragraph
                        [ Font.size <| responsiveVal dProfile 16 20
                        , Font.italic
                        , Font.color <| Element.rgb 0.2 0.2 0.2
                        , Element.paddingEach
                            { left = 15
                            , right = 0
                            , top = 0
                            , bottom = 0
                            }
                        ]
                        [ Element.text explanationText ]
                )
            |> Maybe.withDefault Element.none
        ]


viewGptAssistError : GptAssistError -> Element FrontendMsg
viewGptAssistError gptAssistError =
    Element.paragraph
        [ Element.width Element.fill
        , Font.color <| Element.rgb 1 0 0
        ]
        [ Element.text <| gptAssistErrorToString gptAssistError ]


gptAssistErrorToString : GptAssistError -> String
gptAssistErrorToString gptAssistError =
    case gptAssistError of
        OutOfCredits ->
            "Out of credits! D:"

        ApiProtocolError RateLimited ->
            "OpenAI has rate-limted the app. Sorry about that - please try again later."

        ApiProtocolError (HttpError otherHttpError) ->
            "There was an issue with the AI service provider: " ++ Utils.httpErrorToString otherHttpError

        GptDecodeError decodeError ->
            "The AI did not respond with the expected structure: " ++ decodeError

        GptExpressedError gptsDamnProblemString ->
            "The AI refuses to process the request: \"" ++ gptsDamnProblemString ++ "\""


translateButton : Maybe FrontendMsg -> Element FrontendMsg
translateButton maybeSubmitMsg =
    mainActionButton
        "Translate"
        maybeSubmitMsg


modifyTextButton : String -> Element FrontendMsg
modifyTextButton inputText =
    minorActionButton
        "Edit"
        (Just <| EditTranslation inputText)


newTranslationButton : Element FrontendMsg
newTranslationButton =
    minorActionButton
        "New"
        (Just <| EditTranslation "")
