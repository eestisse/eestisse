module Feedback.View exposing (..)

import Colors
import CommonView exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import EmailAddress
import Maybe.Extra as Maybe
import Responsive exposing (..)
import Types exposing (..)


page : DisplayProfile -> Maybe (Maybe FrontendUserInfo) -> FeedbackFormModel -> Element FrontendMsg
page dProfile maybeMaybeUserInfo feedbackFormModel =
    primaryBox
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding <| responsiveVal dProfile 10 25
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing <| responsiveVal dProfile 15 30
            ]
            [ textHeaderEl dProfile
            , viewMainForm dProfile (Maybe.join maybeMaybeUserInfo) feedbackFormModel
            ]


textHeaderEl : DisplayProfile -> Element FrontendMsg
textHeaderEl dProfile =
    responsiveVal
        dProfile
        (Element.column
            [ Element.spacing 3
            , Element.centerX
            , Font.size 20
            ]
            [ Element.el [ Element.centerX ] <| Element.text "Feedback? Bugs?"
            , Element.el [ Element.centerX ] <| Element.text "Questions, concerns?"
            ]
        )
        (Element.el
            [ Element.centerX
            , Font.size 24
            ]
         <|
            Element.text "Feedback? Bugs? Questions, concerns?"
        )


viewMainForm : DisplayProfile -> Maybe FrontendUserInfo -> FeedbackFormModel -> Element FrontendMsg
viewMainForm dProfile maybeUserInfo feedbackFormModel =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 10
        ]
        [ Element.column
            [ Element.spacing 5
            , Element.width Element.fill
            , Font.size <| responsiveVal dProfile 14 16
            ]
            [ Element.paragraph
                [ Element.width Element.fill
                , Font.center
                ]
                [ Element.text "Leave a message below and I'll check it soon."
                ]
            , Element.paragraph
                [ Element.centerX
                , Font.center
                ]
                [ Element.text "(you can also chat with us "
                , joinDiscordLink "in the Discord"
                , Element.text ")"
                ]
            ]
        , if feedbackFormModel.submitStatus /= Complete then
            textInputEl dProfile feedbackFormModel

          else
            Element.none
        , submitRowEl dProfile maybeUserInfo feedbackFormModel
        ]


textInputEl : DisplayProfile -> FeedbackFormModel -> Element FrontendMsg
textInputEl dProfile feedbackFormModel =
    Input.multiline
        [ Element.width (Element.fill |> Element.maximum 600)
        , Element.centerX
        , Element.height <| Element.px 200
        , Border.rounded 10
        , Element.padding 10
        , Border.width 1
        , Border.color <| Element.rgb 0.8 0.8 1
        , Element.Background.color Colors.white
        ]
        { onChange = \t -> FeedbackFormChanged { feedbackFormModel | textInput = t }
        , text = feedbackFormModel.textInput
        , placeholder = Nothing
        , label = Input.labelHidden "Enter text"
        , spellcheck = True
        }


submitRowEl : DisplayProfile -> Maybe FrontendUserInfo -> FeedbackFormModel -> Element FrontendMsg
submitRowEl dProfile maybeUserInfo feedbackFormModel =
    case feedbackFormModel.submitStatus of
        SubmitWaiting ->
            loadingSnake [ Element.height <| Element.px 40, Element.centerX ]

        Complete ->
            Element.el
                [ Element.padding 5
                , Element.centerX
                , Font.size <| responsiveVal dProfile 18 22
                ]
            <|
                Element.text "Thanks for your feedback!"

        NotSubmitted ->
            let
                maybeSubmitMsg =
                    if feedbackFormModel.textInput /= "" then
                        case maybeUserInfo of
                            Nothing ->
                                if feedbackFormModel.emailInput /= "" then
                                    case EmailAddress.fromString feedbackFormModel.emailInput of
                                        Just emailAddress ->
                                            Just <| TriggerSubmitFeedback False (Just <| EmailAddress.toString emailAddress) feedbackFormModel.textInput

                                        Nothing ->
                                            Nothing

                                else
                                    Just <| TriggerSubmitFeedback False Nothing feedbackFormModel.textInput

                            Just userInfo ->
                                Just <| TriggerSubmitFeedback True (Just userInfo.email) feedbackFormModel.textInput

                    else
                        Nothing
            in
            Element.row
                [ Element.centerX
                , Element.spacing 10
                ]
                [ case maybeUserInfo of
                    Nothing ->
                        emailInput dProfile (Just "email (optional)") feedbackFormModel.emailInput maybeSubmitMsg (\t -> FeedbackFormChanged { feedbackFormModel | emailInput = t })

                    Just userInfo ->
                        Element.none
                , blueButton dProfile [] [] "Submit" maybeSubmitMsg
                ]
