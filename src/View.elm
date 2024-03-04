module View exposing (..)

import Browser
import Element exposing (Element)
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
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
        [ Maybe.map viewTranslationResult model.maybeTranslationResult
            |> Maybe.withDefault Element.none
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


viewTranslationResult : Result GptAssistError Translation -> Element FrontendMsg
viewTranslationResult translationResult =
    case translationResult of
        Err gptAssistError ->
            viewGptAssistError gptAssistError

        Ok translation ->
            let
                partIsSelected part =
                    case translation.selectedExplanation of
                        Just ( selectedPart, _ ) ->
                            part == selectedPart

                        Nothing ->
                            False
            in
            Element.column
                [ Element.spacing 20 ]
                [ Element.paragraph
                    [ Element.width Element.fill ]
                    (translation.inputAndExplanations
                        |> List.map
                            (\( part, explanation ) ->
                                selectPartButton (partIsSelected part) ( part, explanation )
                            )
                        |> List.intersperse (Element.text " ")
                    )
                , Element.paragraph [ Element.width Element.fill ] [ Element.text translation.translation ]
                , translation.selectedExplanation
                    |> Maybe.map Tuple.second
                    |> Maybe.map viewExplanation
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
        ApiProtocolError httpError ->
            "There was an issue with OpenAI's protocol: " ++ Utils.httpErrorToString httpError

        GptDecodeError decodeError ->
            "ChatGPT did not respond with the expected structure: " ++ decodeError

        GptExpressedError gptsDamnProblemString ->
            "ChatGPT refuses to process the request: " ++ gptsDamnProblemString


selectPartButton : Bool -> ( String, String ) -> Element FrontendMsg
selectPartButton partIsSelected ( phrase, explanation ) =
    Element.Input.button
        (if partIsSelected then
            [ Element.Border.width 1
            , Element.Border.color <| Element.rgb 0 0 1
            ]

         else
            []
        )
        { onPress = Just <| ShowExplanation phrase explanation
        , label = Element.text phrase
        }


viewExplanation : String -> Element FrontendMsg
viewExplanation explanation =
    Element.paragraph [] [ Element.text explanation ]
