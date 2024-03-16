module Translate.View exposing (..)

import CommonView exposing (..)
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import List.Extra
import Types exposing (..)
import Utils


page : TranslationPageModel -> Element FrontendMsg
page translationPageModel =
    case translationPageModel of
        InputtingText inputText ->
            viewTranslationPageInput inputText

        RequestSent requestState ->
            viewTranslationPageRequestState requestState


viewTranslationPageInput : String -> Element FrontendMsg
viewTranslationPageInput inputText =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Utils.onEnter
            (if inputText /= "" then
                SubmitText inputText

             else
                NoOpFrontendMsg
            )
        ]
        [ Input.multiline
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 10
            , Border.width 0
            , Font.size translateTextSize
            ]
            { onChange = TextInputChanged
            , text = inputText
            , placeholder =
                Just <|
                    Input.placeholder
                        [ Font.italic
                        ]
                    <|
                        Element.text "Enter Estonian text"
            , label = Input.labelHidden "Enter text"
            , spellcheck = False
            }
        , Element.el
            [ Element.centerX
            , Element.alignBottom
            ]
          <|
            interpretButton (inputText /= "") inputText
        ]


viewTranslationPageRequestState : RequestState -> Element FrontendMsg
viewTranslationPageRequestState requestState =
    Element.column
        [ Element.spacing 5
        , Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        case requestState of
            Waiting inputText animationCounter ->
                [ translationInputTextElement inputText
                , hbreakElement
                , loadingTranslationElement animationCounter
                ]

            RequestComplete completedRequest ->
                let
                    editOrNewButtonsRow =
                        Element.row
                            [ Element.alignBottom
                            , Element.width Element.fill
                            , Element.spacing 20
                            , Element.paddingEach
                                { top = 0
                                , right = 15
                                , left = 15
                                , bottom = 15
                                }
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
                        , translatedTextElement translation.translation
                        , Element.el [ Element.height <| Element.px 8 ] <| Element.none
                        , case completedRequest.maybeSelectedBreakdownPart of
                            Just selectedBreakdownPart ->
                                selectedExplanationElement selectedBreakdownPart

                            Nothing ->
                                clickOnPartsHint
                        , editOrNewButtonsRow
                        ]


translateTextSize : Int
translateTextSize =
    24


translatedTextColor : Element.Color
translatedTextColor =
    Element.rgb 0 0 1


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
                    [ Background.color <| Element.rgb 0.3 0.3 1
                    , Font.color <| Element.rgb 1 1 1
                    ]

                else
                    [ Background.color <| Element.rgb 0.95 0.95 1 ]
               )
        )
        { onPress = Just <| ShowExplanation breakdownPart
        , label = Element.text breakdownPart.estonian
        }


translatedTextElement : String -> Element FrontendMsg
translatedTextElement translatedText =
    Element.el [] <|
        Element.paragraph
            translatedTextStyles
            [ Element.text translatedText ]


loadingTranslationElement : Int -> Element FrontendMsg
loadingTranslationElement animationCounter =
    let
        emojiRow =
            "🤔🤔🤔🧐"
                |> String.toList
                |> List.map String.fromChar
                |> List.Extra.cycle animationCounter
                |> List.map Element.text
                |> Element.row [ Element.spacing 10 ]
    in
    Element.column
        [ Font.size 18
        , Font.color <| Element.rgb 0.5 0.5 0.5
        , Element.centerX
        , Element.spacing 10
        ]
        [ Element.el [ Font.size 24, Element.centerX ] emojiRow
        , Element.el [ Font.italic ] <| Element.text "The robot is thinking carefully..."
        ]


clickOnPartsHint : Element FrontendMsg
clickOnPartsHint =
    Element.el
        [ Element.padding 5
        , Border.width 1
        , Border.rounded 4
        , Border.color <| Element.rgb 0.8 0.8 0
        , Background.color <| Element.rgb 1 1 0.7
        , Element.centerX
        , Font.color <| Element.rgb 0.3 0.3 0.3
        , Font.italic
        ]
    <|
        Element.text "Tap parts of the Estonian text to learn more"


selectedExplanationElement : BreakdownPart -> Element FrontendMsg
selectedExplanationElement breakdownPart =
    Element.column
        [ Element.spacing 5
        , Element.padding 5
        , Border.width 1
        , Border.rounded 4
        , Border.color <| Element.rgb 0.8 0.8 0
        , Background.color <| Element.rgb 1 1 0.7
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
                    , Font.size 18
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
                    , Font.size 18
                    ]
                    [ Element.text breakdownPart.englishTranslation ]
            ]
        , breakdownPart.maybeExplanation
            |> Maybe.map
                (\explanationText ->
                    Element.paragraph
                        [ Font.size 14
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

        ApiProtocolError httpError ->
            "There was an issue with OpenAI's protocol: " ++ Utils.httpErrorToString httpError

        GptDecodeError decodeError ->
            "ChatGPT did not respond with the expected structure: " ++ decodeError

        GptExpressedError gptsDamnProblemString ->
            "ChatGPT refuses to process the request: \"" ++ gptsDamnProblemString ++ "\""


interpretButton : Bool -> String -> Element FrontendMsg
interpretButton enabled inputText =
    mainActionButton
        "Interpret"
        (if enabled then
            Just <| SubmitText inputText

         else
            Nothing
        )


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
