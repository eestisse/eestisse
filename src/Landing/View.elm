module Landing.View exposing (..)

import Colors
import CommonView
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Route
import Types exposing (..)


page : Element FrontendMsg
page =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 25
        ]
        [ Element.el [ Element.centerX ] mainExplainer
        , Element.el [ Element.centerX ] futureFeaturesTeaser
        , Element.el [ Element.centerX ] emailSignupElement
        ]


emphasizedText : String -> Element FrontendMsg
emphasizedText =
    Element.el [ Font.bold, Font.color Colors.darkGreen ] << Element.text


mainExplainer : Element FrontendMsg
mainExplainer =
    let
        italics s =
            Element.el [ Font.italic ] <| Element.text s
    in
    Element.column
        [ Element.spacing 20
        , Element.padding 10
        , Border.width 1
        , Background.color <| Element.rgb 0.95 0.95 1
        , Border.color <| Element.rgb 0.8 0.8 1
        , Border.rounded 6
        ]
        [ CommonView.makeParagraphs
            []
            [ [ CommonView.coloredEestisseText []
              , Element.text " (meaning \""
              , Element.el [ Font.color Colors.teal, Font.bold ] <| Element.text "into "
              , Element.el [ Font.color Colors.mainBlue, Font.bold ] <| Element.text "Estonia"
              , Element.text "\") is a tutoring and assistance tool for anyone learning the Estonian language. "
              ]
            , [ Element.text "The central feature is that of "
              , emphasizedText "deep translation"
              , Element.text ": Estonian text is not only translated better than other tools, but explained piece by piece, with the help of AI."
              ]
            ]
        , Input.button
            [ Element.paddingXY 30 8
            , Element.centerX
            , Background.color <| Element.rgb 0 0 1
            , Font.color <| Element.rgb 1 1 1
            , Font.size 22
            , Border.rounded 10
            , CommonView.madimiFont
            ]
            { onPress = Just <| GotoRoute Route.Translate
            , label = Element.text "Try out Deep Translation"
            }
        , CommonView.makeParagraphs
            []
            [ [ Element.text "As you go through life in Estonia, "
              , CommonView.coloredEestisseText []
              , Element.text " can be your personal tutor, translating and explaining anything you see. "
              , Element.text "Ads, bills, news articles, and Tinder messages all work well."
              ]
            , [ Element.text "Or if you really want to learn fast, pick up an Estonian children's book!"
              ]
            , [ Element.text "Before long you'll find you're picking up common words and getting a feel for the grammar and the intimidating (but interesting!) case system (of which \"eestisse\" is an example, by the way!)"
              ]
            ]
        ]


leftoverText =
    [ [ Element.text "Eestisse (\"Into Estonia\") is designed to help you learn Estonian as you navigate the real world in Estonia." ]
    , [ Element.text "We recommend using it as you encounter any real Estonian text in your life. This could be simple advertisements, snippets of articles, or parts of a text conversation with a new Estonian friend :) If you don't have an easy source of Estonian handy, children's books are wonderful for practice!" ]
    , [ Element.text "Using the power of AI, Eestisse gives a full interpretation, not just a translation. This includes a breakdown that explains the sentence in parts, and includes information about context, grammar, and nuance that normal translations services ignore." ]
    , [ Element.text "Eestisse is particularly helpful with understanding how the case system works!" ]
    , [ Element.text "Once an interpretation is finished, you can you can tap on parts of the original Estonian text to learn more about that part." ]
    , [ Element.text "In the near future, we will be adding English -> Estonian interpretation, interpretation history, and some sort of flashcard memorization mechanic. Stay tuned!" ]
    ]


futureFeaturesTeaser : Element FrontendMsg
futureFeaturesTeaser =
    Element.el
        [ Border.width 2
        , Border.color <| Element.rgb 1 0 0
        ]
    <|
        Element.text "futureFeaturesTeaser"


emailSignupElement : Element FrontendMsg
emailSignupElement =
    Element.el
        [ Border.width 2
        , Border.color <| Element.rgb 1 0 0
        ]
    <|
        Element.text "emailSignupElement"


explainerSubtitleElement : Element FrontendMsg
explainerSubtitleElement =
    [ [ Element.text "Eestisse helps you learn as you translate." ]
    , [ Element.text "It really shines with longer sentences!" ]
    ]
        |> List.map
            (Element.paragraph
                [ Font.center
                , Font.italic
                , Element.spacing 2
                ]
            )
        |> Element.column
            [ Element.centerX
            , Element.padding 5
            , Border.width 1
            , Border.color <| Element.rgb 0.8 0.8 1
            , Background.color <| Element.rgb 0.9 0.9 1
            , Border.rounded 6
            , Element.spacing 10
            ]
