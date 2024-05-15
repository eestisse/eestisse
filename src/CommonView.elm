module CommonView exposing (..)

import Colors
import Config
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import EmailAddress
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Responsive exposing (..)
import Types exposing (..)
import Utils


madimiFont : Attribute msg
madimiFont =
    Font.family
        [ Font.typeface "madimi" ]


robotoFont : Attribute msg
robotoFont =
    Font.family
        [ Font.typeface "roboto" ]


plausibleTrackButtonClick : String -> Attribute FrontendMsg
plausibleTrackButtonClick nameForPlausible =
    let
        weirdClassName =
            "plausible-event-name=" ++ nameForPlausible
    in
    Element.htmlAttribute <|
        Html.Attributes.class weirdClassName


basicButton : DisplayProfile -> List (Attribute msg) -> List (Attribute msg) -> String -> Maybe msg -> ( Element.Color, Element.Color ) -> Element msg
basicButton dProfile extraAttributes extraInnerElAttributes labelText maybeMsg ( bgColorIfActive, textColorIfActive ) =
    Element.el
        ([ Element.paddingXY 15 8
         , Font.size <| responsiveVal dProfile 16 18
         , robotoFont
         , Border.rounded 4
         , noSelectText
         ]
            ++ (case maybeMsg of
                    Just msg ->
                        [ Element.Background.color bgColorIfActive
                        , Font.color textColorIfActive
                        , Element.pointer
                        , Events.onClick msg
                        ]

                    Nothing ->
                        [ Element.Background.color <| Element.rgb 0.5 0.5 0.5
                        , Font.color Colors.white
                        ]
               )
            ++ extraAttributes
        )
    <|
        Element.el extraInnerElAttributes <|
            Element.text labelText


blueButton : DisplayProfile -> List (Attribute msg) -> List (Attribute msg) -> String -> Maybe msg -> Element msg
blueButton dProfile extraAttribtues extraInnerElAttributes labelText maybeMsg =
    basicButton
        dProfile
        extraAttribtues
        extraInnerElAttributes
        labelText
        maybeMsg
        ( Colors.blue, Colors.white )


lightBlueButton : DisplayProfile -> List (Attribute msg) -> List (Attribute msg) -> String -> Maybe msg -> Element msg
lightBlueButton dProfile extraAttributes extraInnerElAttributes labelText maybeMsg =
    basicButton
        dProfile
        ([ Border.width 1, Border.color <| Element.rgb 0.7 0.7 1 ] ++ extraAttributes)
        extraInnerElAttributes
        labelText
        maybeMsg
        ( Colors.lightBlue, Colors.black )


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


makeParagraphs : List (Attribute msg) -> List (List (Element msg)) -> Element msg
makeParagraphs extraAttributes =
    List.map
        (Element.paragraph [ Element.spacing 2 ])
        >> Element.column
            ([ Element.spacing 20
             ]
                ++ extraAttributes
            )


coloredEestisseText : List (Attribute msg) -> Element msg
coloredEestisseText extraAttributes =
    Element.row
        ([ madimiFont ] ++ extraAttributes)
        [ Element.el [ Font.color <| Colors.mainBlue ] <| Element.text "eesti"
        , Element.el [ Font.color <| Colors.teal ] <| Element.text "sse"
        ]


linkAttributes : List (Attribute msg)
linkAttributes =
    [ Font.color Colors.mainBlue
    , Font.underline
    ]


newTabLink : List (Attribute msg) -> String -> String -> Element msg
newTabLink extraAttributes url labelText =
    Element.newTabLink
        (linkAttributes ++ extraAttributes)
        { url = url
        , label = Element.text labelText
        }


htmlId : String -> Attribute msg
htmlId idStr =
    Element.htmlAttribute <| Html.Attributes.id idStr


primaryBox : List (Attribute msg) -> Element msg -> Element msg
primaryBox =
    primaryBoxCustomColors
        Colors.calmTeal
        (Element.rgb 0.95 0.95 1)


primaryBoxCustomColors : Element.Color -> Element.Color -> List (Attribute msg) -> Element msg -> Element msg
primaryBoxCustomColors borderColor backgroundColor extraAttributes innerEl =
    Element.el
        ([ Element.padding 10
         , Border.rounded 30
         , Border.shadow
            { offset = ( 5, 5 )
            , size = 5
            , blur = 10
            , color = Element.rgba 0 0 0 0.3
            }
         , Border.color borderColor
         , Border.width 10
         , Element.Background.color backgroundColor
         ]
            ++ extraAttributes
        )
        innerEl


scrollbarYEl : List (Attribute msg) -> Element msg -> Element msg
scrollbarYEl attrs body =
    Element.row
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.clip
        ]
        [ Element.el
            ([ Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
             , Element.htmlAttribute <| Html.Attributes.style "top" "0"
             , Element.htmlAttribute <| Html.Attributes.style "right" "0"
             , Element.htmlAttribute <| Html.Attributes.style "bottom" "0"
             , Element.htmlAttribute <| Html.Attributes.style "left" "0"
             , Element.scrollbarY
             ]
                ++ attrs
            )
            body
        ]


basicShadow : Attribute msg
basicShadow =
    Border.shadow
        { offset = ( 3, 3 )
        , size = 1
        , blur = 5
        , color = Element.rgb 0.8 0.8 0.8
        }


bulletPointList : Int -> List (Attribute msg) -> List (Element msg) -> Element msg
bulletPointList bulletFontSizeAndSpacing attributes items =
    Element.column
        ([ Element.spacing (bulletFontSizeAndSpacing // 2) ] ++ attributes)
        (items
            |> List.map
                (\item ->
                    Element.row [ Element.spacing bulletFontSizeAndSpacing ]
                        [ Element.el [ Element.alignTop ] <| Element.text "•"
                        , item
                        ]
                )
        )


textWithCutoff : String -> Element msg
textWithCutoff s =
    Element.html <|
        Html.div
            [ Html.Attributes.style "text-overflow" "ellipsis"
            , Html.Attributes.style "white-space" "nowrap"
            , Html.Attributes.style "overflow" "hidden"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "flex-basis" "auto"
            ]
            [ Html.text s ]


actionLink : String -> msg -> Element msg
actionLink text msg =
    Element.el
        (linkAttributes ++ [ Element.pointer, Events.onClick msg ])
    <|
        Element.text text


signinElement : DisplayProfile -> SigninModel -> Element FrontendMsg
signinElement dProfile signinModel =
    case signinModel.emailFormMode of
        Inactive ->
            responsiveVal dProfile
                Element.column
                Element.row
                [ Element.centerX
                , Element.spacing 10
                ]
                [ googleSigninButton dProfile
                , magicLinkButton dProfile
                ]

        InputtingEmail input ->
            emailInputForm dProfile input

        InputtingCode inputtingCodeModel ->
            magicCodeInputForm dProfile inputtingCodeModel

        CodeSubmitted email ->
            magicCodeWaiting dProfile email


emailInputForm : DisplayProfile -> String -> Element FrontendMsg
emailInputForm dProfile input =
    let
        submitMsgIfEmailIsValid =
            EmailAddress.fromString input
                |> Maybe.map SendEmailToBackendForCode
    in
    Element.column
        [ Element.spacing <| responsiveVal dProfile 20 20
        , Element.centerX
        ]
        [ Element.column
            [ Element.spacing 5
            , Font.size <| responsiveVal dProfile 16 18
            ]
            (case dProfile of
                Mobile ->
                    [ Element.text "Enter your email."
                    , Element.text "We'll send you a code to login."
                    ]

                Desktop ->
                    [ Element.text "Enter your email, and we'll send you a code to login."
                    ]
            )
        , Element.row
            [ Element.centerX
            , Element.spacing 10
            ]
            [ sleekTextInput dProfile
                [ Element.width <| Element.px 180
                , onEnter (submitMsgIfEmailIsValid |> Maybe.withDefault NoOpFrontendMsg)
                ]
                { onChange = \t -> ChangeEmailForm <| InputtingEmail t
                , text = input
                , placeholder = Just <| Input.placeholder [ Font.color Colors.gray ] <| Element.text "you@something.com"
                , label = Input.labelHidden "email input"
                }
            , blueButton dProfile [] [] "send code" submitMsgIfEmailIsValid
            ]
        ]


magicCodeInputForm : DisplayProfile -> InputtingCodeModel -> Element FrontendMsg
magicCodeInputForm dProfile inputtingCodeModel =
    let
        submitMsgIfNonempty =
            if inputtingCodeModel.input == "" then
                Nothing

            else
                Just <| SubmitCodeClicked inputtingCodeModel.emailAddress inputtingCodeModel.input
    in
    Element.column
        [ Element.spacing <| responsiveVal dProfile 20 20
        , Element.centerX
        ]
        [ magicCodeFormHeader dProfile inputtingCodeModel.emailAddress
        , case inputtingCodeModel.maybeError of
            Nothing ->
                Element.none

            Just IncorrectCode ->
                Element.el
                    [ Element.centerX
                    , Font.color Colors.errorTextRed
                    ]
                <|
                    Element.text "Code is not correct"

            Just CodeExpired ->
                Element.row
                    [ Element.centerX
                    , Font.color Colors.errorTextRed
                    ]
                    [ Element.text "Code has expired. "
                    , actionLink "send another" <| SendEmailToBackendForCode <| inputtingCodeModel.emailAddress
                    ]
        , Element.row
            [ Element.centerX
            , Element.spacing 10
            ]
            [ sleekTextInput
                dProfile
                [ Element.width <| Element.px 100
                , onEnter (submitMsgIfNonempty |> Maybe.withDefault NoOpFrontendMsg)
                ]
                { onChange = \t -> ChangeEmailForm <| InputtingCode { inputtingCodeModel | input = t }
                , text = inputtingCodeModel.input
                , placeholder = Nothing
                , label = Input.labelHidden "code input"
                }
            , blueButton dProfile [] [] "submit" submitMsgIfNonempty
            ]
        ]


magicCodeWaiting : DisplayProfile -> EmailAddress.EmailAddress -> Element FrontendMsg
magicCodeWaiting dProfile email =
    Element.column
        [ Element.spacing <| responsiveVal dProfile 20 20
        , Element.centerX
        ]
        [ magicCodeFormHeader dProfile email
        , loadingSnake
            [ Element.centerX
            , Element.height <| Element.px 40
            ]
        ]


magicCodeFormHeader : DisplayProfile -> EmailAddress.EmailAddress -> Element FrontendMsg
magicCodeFormHeader dProfile emailAddress =
    Element.column
        [ Element.spacing 5
        , Font.size <| responsiveVal dProfile 16 18
        ]
        [ Element.text <| "A code has been sent to " ++ EmailAddress.toString emailAddress ++ "."
        , Element.text <| "Enter it here within " ++ Config.emailCodeExpirationString ++ "."
        ]


googleSigninButton : DisplayProfile -> Element FrontendMsg
googleSigninButton dProfile =
    Input.button
        []
        { onPress =
            Just <|
                GoogleSigninRequested
        , label =
            Element.image
                []
                { src = "/google-signin-button.png"
                , description = "Google"
                }
        }


magicLinkButton : DisplayProfile -> Element FrontendMsg
magicLinkButton dProfile =
    Input.button
        [ Element.height <| Element.px 40
        , Element.Background.color Colors.white
        , Border.width 1
        , Border.color <| Element.rgb 0.4 0.4 0.4
        , Border.rounded 4
        , Element.paddingXY 13 0
        , Font.size 14
        , Font.bold
        ]
        { onPress = Just EmailSigninRequested
        , label =
            Element.row
                [ Element.spacing 10 ]
                [ Element.image
                    [ Element.height <| Element.px 26 ]
                    { src = "/email.png"
                    , description = "email"
                    }
                , Element.text "Email Magic Link"
                ]
        }


loadingSnake : List (Attribute msg) -> Element msg
loadingSnake attributes =
    Element.image
        attributes
        { src = "/loading-snake-io.gif"
        , description = "loading"
        }


noSelectText : Attribute msg
noSelectText =
    Html.Attributes.class "noselect" |> Element.htmlAttribute


sleekTextInput :
    DisplayProfile
    -> List (Attribute msg)
    -> { onChange : String -> msg, text : String, placeholder : Maybe (Input.Placeholder msg), label : Input.Label msg }
    -> Element msg
sleekTextInput dProfile extraAttributes buttonStuff =
    Input.text
        ([ Element.height <| Element.px <| responsiveVal dProfile 32 36
         , Element.padding <| responsiveVal dProfile 5 7
         ]
            ++ extraAttributes
        )
        buttonStuff


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )
