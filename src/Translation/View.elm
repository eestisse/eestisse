module Translation.View exposing (..)

import Colors
import CommonView exposing (..)
import Element exposing (..)
import Element.Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import List.Extra
import Maybe.Extra as Maybe
import Responsive exposing (..)
import Route
import Time
import Time.Extra
import Translation.Types exposing (..)
import Types exposing (..)
import Utils


viewLoadingTranslationPage : DisplayProfile -> Element FrontendMsg
viewLoadingTranslationPage dProfile =
    primaryBox
        [ Element.width Element.fill
        , Element.padding 15
        ]
    <|
        Element.el [ Element.centerX ] <|
            Element.text "Fetching translation..."


viewDoTranslatePage : DisplayProfile -> Maybe PublicCreditsInfo -> Time.Posix -> Maybe (Maybe FrontendUserInfo) -> DoTranslateModel -> Bool -> Int -> Element FrontendMsg
viewDoTranslatePage dProfile maybePublicCreditsInfo now maybeMaybeUserInfo doTranslateModel publicConsentChecked animationCounter =
    let
        maybeUserInfo =
            Maybe.join maybeMaybeUserInfo
    in
    case doTranslateModel.state of
        Inputting ->
            if maybeFrontendUserSignupComplete maybeUserInfo then
                viewTranslateInputPage dProfile maybeUserInfo doTranslateModel.input publicConsentChecked

            else
                case maybePublicCreditsInfo of
                    Just publicCreditsInfo ->
                        if publicCreditsInfo.current == 0 then
                            viewNoCreditsPage dProfile publicCreditsInfo now

                        else
                            viewTranslateInputPage dProfile maybeUserInfo doTranslateModel.input publicConsentChecked

                    Nothing ->
                        viewTranslateInputPage dProfile maybeUserInfo doTranslateModel.input publicConsentChecked

        TranslateRequestSubmitted ->
            viewWaitingForResponsePage dProfile doTranslateModel.input animationCounter

        Error gptAssistError ->
            viewTranslateErrorPage dProfile doTranslateModel.input gptAssistError


viewTranslateErrorPage : DisplayProfile -> String -> GptAssistError -> Element FrontendMsg
viewTranslateErrorPage dProfile input gptAssistError =
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
            [ translationInputTextElement input
            , hbreakElement
            , viewGptAssistError gptAssistError
            , editOrNewButtonsRow dProfile input
            ]


viewGptAssistError : GptAssistError -> Element FrontendMsg
viewGptAssistError gptAssistError =
    Element.paragraph
        [ Font.color <| Element.rgb 1 0 0
        , Element.width Element.fill
        , Element.height <| Element.px 400
        , Element.scrollbarY
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


viewWaitingForResponsePage : DisplayProfile -> String -> Int -> Element FrontendMsg
viewWaitingForResponsePage dProfile input animationCounter =
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
            [ Element.el [ Element.centerX ] <| translationInputTextElement input
            , Element.row
                [ Element.width Element.fill ]
                [ hbreakElement
                , loadingEmojiAnimation animationCounter
                , hbreakElement
                ]
            , abortButton dProfile input
            ]


translationInputTextElement : String -> Element FrontendMsg
translationInputTextElement inputText =
    Element.el [] <|
        Element.paragraph
            [ Font.size translateTextSize
            , Element.width Element.fill
            ]
            [ Element.text inputText ]


abortButton : DisplayProfile -> String -> Element FrontendMsg
abortButton dProfile inputText =
    lightBlueButton dProfile [] [] "Abort and Edit" (Just <| EditTranslation inputText)


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


viewTranslateInputPage : DisplayProfile -> Maybe FrontendUserInfo -> String -> Bool -> Element FrontendMsg
viewTranslateInputPage dProfile maybeAuthedUserInfo input publicConsentChecked =
    let
        submitMsgIfEnabled =
            if input /= "" then
                Just <| SubmitText publicConsentChecked input

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
                    { onChange = TranslationInputChanged
                    , text = input
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
                    { onChange = PublicConsentChecked
                    , icon = Input.defaultCheckbox
                    , checked = publicConsentChecked
                    , label =
                        Input.labelRight
                            [ Font.size <| responsiveVal dProfile 18 20
                            , Font.color <| Element.rgb 0.3 0.3 0.3
                            ]
                        <|
                            Element.text "Share translation publicly"
                    }
                , Element.el
                    [ Element.width Element.fill
                    , Element.height <| Element.px 50
                    ]
                  <|
                    if publicConsentChecked || maybeFrontendUserSignupComplete maybeAuthedUserInfo then
                        Element.el [ Element.centerX ] <| translateButton dProfile submitMsgIfEnabled

                    else
                        Element.column
                            [ Element.spacing 5
                            , Font.color <| Element.rgb 1 0 0
                            , Element.centerX
                            ]
                            [ Element.row
                                [ Element.centerX ]
                                [ Element.text "You must have an "
                                , actionLink "active membership" UserIntent_ActivateMembership
                                ]
                            , Element.el [ Element.centerX ] <| Element.text "to process translations privately."
                            ]
                ]
            ]


viewTranslationPage : DisplayProfile -> TranslationRecord -> ViewTranslationModel -> Element FrontendMsg
viewTranslationPage dProfile translationRecord viewTranslationModel =
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
            [ translationInputButtonsElement translationRecord.translation.breakdown viewTranslationModel.maybeSelectedBreakdownPartId
            , hbreakElement
            , englishTextElement <|
                case translationRecord.translation.translatedTo of
                    English ->
                        translationRecord.translation.translatedText

                    Estonian ->
                        translationRecord.input
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
                        case viewTranslationModel.maybeSelectedBreakdownPartId of
                            Just selectedBreakdownPartId ->
                                case List.Extra.getAt selectedBreakdownPartId translationRecord.translation.breakdown of
                                    Just breakdownPart ->
                                        selectedExplanationElement dProfile breakdownPart

                                    Nothing ->
                                        clickOnPartsHint dProfile

                            Nothing ->
                                clickOnPartsHint dProfile
            , editOrNewButtonsRow dProfile translationRecord.input
            ]


editOrNewButtonsRow : DisplayProfile -> String -> Element FrontendMsg
editOrNewButtonsRow dProfile input =
    Element.row
        [ Element.alignBottom
        , Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.el [ Element.alignLeft ] <| modifyTextButton dProfile input
        , Element.el [ Element.alignRight ] <| newTranslationButton dProfile
        ]


translationInputButtonsElement : Breakdown -> Maybe Int -> Element FrontendMsg
translationInputButtonsElement breakdown maybeSelectedBreakdownPartId =
    Element.paragraph
        [ Font.size translateTextSize
        , Element.width Element.fill
        ]
        (breakdown
            |> List.indexedMap
                (\i breakdownPart ->
                    selectPartButton (maybeSelectedBreakdownPartId == Just i) i breakdownPart
                )
            |> List.intersperse (Element.text " ")
        )


selectPartButton : Bool -> Int -> BreakdownPart -> Element FrontendMsg
selectPartButton partIsSelected id breakdownPart =
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
        { onPress = Just <| ShowExplanation id
        , label = Element.text breakdownPart.estonian
        }


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


englishTextElement : String -> Element FrontendMsg
englishTextElement translatedText =
    Element.el [] <|
        Element.paragraph
            translatedTextStyles
            [ Element.text translatedText ]


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


modifyTextButton : DisplayProfile -> String -> Element FrontendMsg
modifyTextButton dProfile inputText =
    lightBlueButton
        dProfile
        []
        []
        "Edit"
        (Just <| EditTranslation inputText)


newTranslationButton : DisplayProfile -> Element FrontendMsg
newTranslationButton dProfile =
    lightBlueButton
        dProfile
        []
        []
        "New"
        (Just <| EditTranslation "")


translateButton : DisplayProfile -> Maybe FrontendMsg -> Element FrontendMsg
translateButton dProfile maybeSubmitMsg =
    blueButton
        dProfile
        [ Element.paddingXY 30 8 ]
        [ Font.size <| responsiveVal dProfile 20 24 ]
        "Translate"
        maybeSubmitMsg


viewNoCreditsPage : DisplayProfile -> PublicCreditsInfo -> Time.Posix -> Element FrontendMsg
viewNoCreditsPage dProfile publicCreditsInfo now =
    primaryBox
        [ Element.width Element.fill ]
    <|
        Element.column
            [ Element.centerX
            , Element.spacing 20
            , Font.size <| responsiveVal dProfile 14 18
            ]
            [ Element.paragraph [ Font.size <| responsiveVal dProfile 20 24 ] [ Element.text "No more public translate credits available!" ]
            , Element.column
                [ Element.spacing 10 ]
                [ Element.text "You can:"
                , bulletPointList
                    (responsiveVal dProfile 14 18)
                    [ Element.paddingEach
                        { left = 10
                        , right = 0
                        , bottom = 0
                        , top = 0
                        }
                    ]
                    [ Element.paragraph []
                        [ actionLink "Subscribe (3€/month)" UserIntent_ActivateMembership
                        , Element.text " for unlimited personal translations and more!"
                        ]
                    , Element.paragraph []
                        [ actionLink "Check out other users' translations" <| GotoRouteAndAnimate Route.Browse
                        ]
                    , Element.paragraph []
                        [ Element.text <| "Wait until a few more public translation credits trickle in (" ++ newCreditsArrivingString publicCreditsInfo now ++ ")"
                        ]
                    ]
                ]
            ]


newCreditsArrivingString : PublicCreditsInfo -> Time.Posix -> String
newCreditsArrivingString publicCreditsInfo now =
    let
        totalSecondsDiff =
            Time.Extra.diff Time.Extra.Second Time.utc now publicCreditsInfo.nextRefresh

        minutesLeft =
            totalSecondsDiff // 60

        secondsLeft =
            totalSecondsDiff - (minutesLeft * 60)

        timeStr =
            String.fromInt minutesLeft ++ ":" ++ String.padLeft 2 '0' (String.fromInt secondsLeft)
    in
    String.fromInt publicCreditsInfo.refreshAmount ++ " more arriving in " ++ timeStr
