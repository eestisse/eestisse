module Translation.View exposing (..)

import CommonView exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
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
                        , Maybe.map selectedExplanationElement completedRequest.maybeSelectedBreakdownPart
                            |> Maybe.withDefault Element.none
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
    [ Element.Font.size translateTextSize
    , Element.width Element.fill
    , Element.Font.color translatedTextColor
    , Element.Font.italic
    ]


translationInputTextElement : String -> Element FrontendMsg
translationInputTextElement inputText =
    Element.el [] <|
        Element.paragraph
            [ Element.Font.size translateTextSize
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
    Element.el [] <|
        Element.paragraph
            [ Element.Font.size translateTextSize
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
    Element.Input.button
        ([ Element.Border.width 1
         , Element.padding 2
         , Element.Border.color <| Element.rgb 0.5 0.5 1
         ]
            ++ (if partIsSelected then
                    [ Element.Border.width 1
                    , Element.Border.color <| Element.rgb 0 0 1
                    ]

                else
                    [ Element.Border.dashed ]
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
        [ Element.Font.size 18
        , Element.Font.color <| Element.rgb 0.5 0.5 0.5
        , Element.centerX
        , Element.spacing 10
        ]
        [ Element.el [ Element.Font.size 24, Element.centerX ] emojiRow
        , Element.el [ Element.Font.italic ] <| Element.text "The robot is thinking carefully..."
        ]


selectedExplanationElement : BreakdownPart -> Element FrontendMsg
selectedExplanationElement breakdownPart =
    Element.column
        [ Element.spacing 5
        , Element.padding 5
        , Element.Border.width 1
        , Element.Border.rounded 4
        , Element.Border.color <| Element.rgb 0.8 0.8 0
        , Element.Background.color <| Element.rgb 1 1 0.7
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
                Element.el
                    [ Element.alignRight
                    , Element.Font.bold
                    , Element.Font.size 18
                    ]
                <|
                    Element.text breakdownPart.estonian
            , vbreakElement
            , Element.el
                [ Element.width Element.fill
                ]
              <|
                Element.el
                    [ Element.alignLeft
                    , Element.Font.italic
                    , Element.Font.color translatedTextColor
                    , Element.Font.size 18
                    ]
                <|
                    Element.text breakdownPart.englishTranslation
            ]
        , breakdownPart.maybeExplanation
            |> Maybe.map
                (\explanationText ->
                    Element.paragraph
                        [ Element.Font.size 14
                        , Element.Font.italic
                        , Element.Font.color <| Element.rgb 0.2 0.2 0.2
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
        , Element.Font.color <| Element.rgb 1 0 0
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
        [ Element.Input.multiline
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 10
            , Element.Border.width 0
            , Element.Font.size translateTextSize
            , Element.Events.onFocus HideExplainer
            ]
            { onChange = TextInputChanged
            , text = inputText
            , placeholder =
                Just <|
                    Element.Input.placeholder
                        [ Element.Font.italic
                        ]
                    <|
                        Element.text "Enter Estonian text"
            , label = Element.Input.labelHidden "Enter text"
            , spellcheck = False
            }
        , Element.el
            [ Element.centerX
            , Element.alignBottom
            ]
          <|
            interpretButton (inputText /= "") inputText
        ]


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
