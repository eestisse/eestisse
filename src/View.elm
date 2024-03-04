module View exposing (..)

import Browser
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Json.Decode
import List
import Types exposing (..)
import Utils


root : FrontendModel -> Browser.Document FrontendMsg
root model =
    { title = ""
    , body =
        [ Element.layout [] <| viewBody model ]
    }


viewBody : FrontendModel -> Element FrontendMsg
viewBody model =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.width <| Element.maximum 800 Element.fill
        , Element.height Element.fill
        , Element.Border.width 1
        , Element.Border.color <| Element.rgb 0 0 0
        , Element.Border.rounded 10
        , Element.padding 10
        , Element.Font.size 16
        ]
        [ viewRequestState model.requestState
        , viewTextInputMode model.textInput
        ]


viewTextInputMode : String -> Element FrontendMsg
viewTextInputMode inputText =
    Element.row
        [ Element.width Element.fill
        , Element.alignBottom
        ]
        [ Element.Input.text
            [ Element.width Element.fill ]
            { onChange = TextInputChanged
            , text = inputText
            , placeholder = Nothing
            , label = Element.Input.labelHidden "Enter text"
            }
        , submitButton inputText
        ]


submitButton : String -> Element FrontendMsg
submitButton inputText =
    Element.Input.button
        []
        { onPress = Just <| SubmitText inputText
        , label = Element.text "Go"
        }


viewRequestState : RequestState -> Element FrontendMsg
viewRequestState requestState =
    Element.column
        [ Element.spacing 5
        , Element.width Element.fill
        ]
    <|
        case requestState of
            NotSubmitted ->
                []

            Loading inputText ->
                [ justInputElement inputText
                , loadingTranslationElement
                ]

            RequestComplete completedRequest ->
                case completedRequest.translationResult of
                    Err gptAssistError ->
                        [ justInputElement completedRequest.inputText
                        , viewGptAssistError gptAssistError
                        ]

                    Ok translation ->
                        [ inputAsButtonsElement translation.breakdown completedRequest.maybeSelectedBreakdownPart
                        , translatedTextElement translation.translation
                        , Element.el [ Element.height <| Element.px 8 ] <| Element.none
                        , Maybe.map selectedExplanationElement completedRequest.maybeSelectedBreakdownPart
                            |> Maybe.withDefault Element.none
                        ]


inputTextStyles : List (Attribute FrontendMsg)
inputTextStyles =
    [ Element.width Element.fill ]


translatedTextColor : Element.Color
translatedTextColor =
    Element.rgb 0 0 1


translatedTextStyles : List (Attribute FrontendMsg)
translatedTextStyles =
    [ Element.width Element.fill
    , Element.Font.color translatedTextColor
    , Element.Font.italic
    ]


justInputElement : String -> Element FrontendMsg
justInputElement inputText =
    Element.paragraph
        inputTextStyles
        [ Element.text inputText ]


inputAsButtonsElement : Breakdown -> Maybe BreakdownPart -> Element FrontendMsg
inputAsButtonsElement breakdown maybeSelectedBreakdownPart =
    let
        partIsSelected estonian =
            case maybeSelectedBreakdownPart of
                Just selectedBreakdownPart ->
                    estonian == selectedBreakdownPart.estonian

                Nothing ->
                    False
    in
    Element.paragraph
        inputTextStyles
        (breakdown
            |> List.map
                (\breakdownPart ->
                    selectPartButton (partIsSelected breakdownPart.estonian) breakdownPart
                )
            |> List.intersperse (Element.text " ")
        )


translatedTextElement : String -> Element FrontendMsg
translatedTextElement translatedText =
    Element.paragraph
        translatedTextStyles
        [ Element.text translatedText ]


loadingTranslationElement : Element FrontendMsg
loadingTranslationElement =
    Element.el
        [ Element.Font.italic
        , Element.Font.size 14
        , Element.Font.color <| Element.rgb 0.5 0.5 0.5
        ]
        (Element.text "The robot is translating...")


selectedExplanationElement : BreakdownPart -> Element FrontendMsg
selectedExplanationElement breakdownPart =
    Element.column
        [ Element.spacing 5
        , Element.padding 5
        , Element.Border.width 1
        , Element.Border.rounded 4
        , Element.Border.color <| Element.rgb 0.8 0.8 0
        , Element.Background.color <| Element.rgb 1 1 0.7
        ]
        [ Element.row
            []
            [ Element.el
                [ Element.Font.bold
                ]
              <|
                Element.text <|
                    breakdownPart.estonian
                        ++ ":   "
            , Element.el
                [ Element.Font.color translatedTextColor
                , Element.Font.italic
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


hbreakElement : Element FrontendMsg
hbreakElement =
    Element.text "---"


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
        ApiProtocolError httpError ->
            "There was an issue with OpenAI's protocol: " ++ Utils.httpErrorToString httpError

        GptDecodeError decodeError ->
            "ChatGPT did not respond with the expected structure: " ++ decodeError

        GptExpressedError gptsDamnProblemString ->
            "ChatGPT refuses to process the request: " ++ gptsDamnProblemString


selectPartButton : Bool -> BreakdownPart -> Element FrontendMsg
selectPartButton partIsSelected breakdownPart =
    Element.Input.button
        (if partIsSelected then
            [ Element.Border.width 1
            , Element.Border.color <| Element.rgb 0 0 1
            ]

         else
            []
        )
        { onPress = Just <| ShowExplanation breakdownPart
        , label = Element.text breakdownPart.estonian
        }
