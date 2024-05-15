module CommonView exposing (..)

import Colors
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import EmailAddress
import Html
import Html.Attributes
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
            Element.row
                [ Element.centerX
                , Element.spacing 10
                ]
                [ googleSigninButton dProfile
                , magicLinkButton dProfile
                ]

        InputtingEmail input ->
            emailInputForm dProfile input

        InputtingCode email input ->
            magicCodeInputForm dProfile email input

        CodeSubmitted ->
            loadingSnake []


emailInputForm : DisplayProfile -> String -> Element FrontendMsg
emailInputForm dProfile input =
    let
        submitMsgIfEmailIsValid =
            EmailAddress.fromString input
                |> Maybe.map SubmitEmailClicked
    in
    Element.row
        [ Element.centerX
        , Element.spacing 10
        ]
        [ sleekTextInput dProfile
            [ Element.width <| Element.px 180 ]
            { onChange = \t -> ChangeEmailForm <| InputtingEmail t
            , text = input
            , placeholder = Just <| Input.placeholder [ Font.color Colors.gray ] <| Element.text "you@something.com"
            , label = Input.labelHidden "email input"
            }
        , blueButton dProfile [] [] "send code" submitMsgIfEmailIsValid
        ]


magicCodeInputForm : DisplayProfile -> EmailAddress.EmailAddress -> String -> Element FrontendMsg
magicCodeInputForm dProfile emailAddress input =
    let
        submitMsgIfNonempty =
            if input == "" then
                Nothing

            else
                Just <| SubmitCodeClicked emailAddress input
    in
    Element.row
        [ Element.centerX
        , Element.spacing 10
        ]
        [ sleekTextInput
            dProfile
            [ Element.width <| Element.px 100 ]
            { onChange = \t -> ChangeEmailForm <| InputtingCode emailAddress t
            , text = input
            , placeholder = Nothing
            , label = Input.labelHidden "code input"
            }
        , blueButton dProfile [] [] "submit" submitMsgIfNonempty
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
        []
        { onPress = Just EmailSigninRequested
        , label =
            Element.el
                []
            <|
                Element.text "Email Magic Link"
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
