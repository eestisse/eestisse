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
import List.Extra
import Route exposing (Route)
import Types exposing (..)
import Utils


root : FrontendModel -> Browser.Document FrontendMsg
root model =
    { title = "Eestisse"
    , body =
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Element.width (Element.fill |> Element.maximum 700)
            , Element.height Element.fill
            ]
          <|
            view model
        ]
    }


view : FrontendModel -> Element FrontendMsg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Font.size 16
        , Element.spacing 25
        , Element.padding 10
        ]
        [ titleElement
        , case model.route of
            Route.Translate ->
                viewTranslationPage model.translationPageModel

            Route.History ->
                viewHistoryPage

            Route.Badroute ->
                viewBadRoute
        ]


titleElement : Element FrontendMsg
titleElement =
    Element.row
        [ Element.centerX
        , Element.Font.size 28
        , Element.Font.italic
        , madimiFont
        ]
        [ Element.el [ Element.Font.color <| Element.rgb 0.2 0.2 1 ] <| Element.text "eesti"
        , Element.el [ Element.Font.color <| Element.rgb 0 0.5 0.8 ] <| Element.text "sse"
        ]


viewTranslationPage : TranslationPageModel -> Element FrontendMsg
viewTranslationPage translationPageModel =
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


hbreakElement : Element FrontendMsg
hbreakElement =
    breakElement False


vbreakElement : Element FrontendMsg
vbreakElement =
    breakElement True


breakElement : Bool -> Element FrontendMsg
breakElement isVertical =
    let
        outerEl =
            if isVertical then
                Element.el [ Element.height <| Element.fillPortion 1 ] Element.none

            else
                Element.el [ Element.width <| Element.fillPortion 1 ] Element.none

        innerEl =
            if isVertical then
                Element.el
                    [ Element.height <| Element.fillPortion 6
                    , Element.width <| Element.px 2
                    , Element.Background.color <| Element.rgb 0.7 0.7 1
                    ]
                    Element.none

            else
                Element.el
                    [ Element.width <| Element.fillPortion 6
                    , Element.height <| Element.px 3
                    , Element.Background.color <| Element.rgb 0.7 0.7 1
                    ]
                <|
                    Element.none
    in
    (if isVertical then
        Element.column
            [ Element.height Element.fill
            , Element.paddingXY 5 0
            ]

     else
        Element.row
            [ Element.width Element.fill
            , Element.paddingXY 0 7
            ]
    )
        [ outerEl
        , innerEl
        , outerEl
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
            ]
            { onChange = TextInputChanged
            , text = inputText
            , placeholder = Just <| Element.Input.placeholder [ Element.Font.italic ] <| Element.text "Enter Estonian text to translate"
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


mainActionButton : String -> Maybe FrontendMsg -> Element FrontendMsg
mainActionButton labelText maybeMsg =
    Element.Input.button
        [ Element.paddingXY 30 8
        , Element.Background.color <|
            case maybeMsg of
                Just _ ->
                    Element.rgb 0 0 1

                _ ->
                    Element.rgb 0.5 0.5 0.5
        , Element.Font.color <| Element.rgb 1 1 1
        , Element.Font.size 26
        , Element.Border.roundEach
            { topLeft = 20
            , bottomRight = 20
            , topRight = 3
            , bottomLeft = 3
            }
        , madimiFont
        ]
        { onPress = maybeMsg
        , label = Element.text labelText
        }


minorActionButton : String -> Maybe FrontendMsg -> Element FrontendMsg
minorActionButton labelText maybeMsg =
    Element.Input.button
        [ Element.paddingXY 15 8
        , Element.Border.width 1
        , Element.Border.color <| Element.rgb 0.5 0.5 1
        , Element.Background.color <| Element.rgb 0.9 0.9 1
        , Element.Font.size 24
        , Element.Font.color <| Element.rgb 0 0 0.5
        , Element.Border.rounded 10
        ]
        { onPress = maybeMsg
        , label = Element.el [ Element.centerX, Element.centerY ] <| Element.text labelText
        }


viewHistoryPage : Element FrontendMsg
viewHistoryPage =
    Element.text "history page"


viewBadRoute : Element FrontendMsg
viewBadRoute =
    Element.text "bad route page"


madimiFont : Attribute FrontendMsg
madimiFont =
    Element.Font.family
        [ Element.Font.typeface "madimi" ]
