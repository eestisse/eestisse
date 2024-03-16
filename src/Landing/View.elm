module Landing.View exposing (..)

import CommonView
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Types exposing (..)


page : Element FrontendMsg
page =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 25
        ]
        [ Element.el [ Element.centerX ] mainExplainer
        , Element.el [ Element.centerX ] tryDeepTranslateButton
        , Element.el [ Element.centerX ] futureFeaturesTeaser
        , Element.el [ Element.centerX ] emailSignupElement
        ]


mainExplainer : Element FrontendMsg
mainExplainer =
    CommonView.makeParagraphs
        [ Element.padding 10
        , Border.width 1
        , Background.color <| Element.rgb 0.95 0.95 1
        , Border.color <| Element.rgb 0.8 0.8 1
        , Border.rounded 6
        ]
        [ [ Element.text "Eestisse (\"Into Estonia\") is designed to help you learn Estonian as you navigate the real world in Estonia." ]
        , [ Element.text "We recommend using it as you encounter any real Estonian text in your life. This could be simple advertisements, snippets of articles, or parts of a text conversation with a new Estonian friend :) If you don't have an easy source of Estonian handy, children's books are wonderful for practice!" ]
        , [ Element.text "Using the power of AI, Eestisse gives a full interpretation, not just a translation. This includes a breakdown that explains the sentence in parts, and includes information about context, grammar, and nuance that normal translations services ignore." ]
        , [ Element.text "Eestisse is particularly helpful with understanding how the case system works!" ]
        , [ Element.text "Once an interpretation is finished, you can you can tap on parts of the original Estonian text to learn more about that part." ]
        , [ Element.text "In the near future, we will be adding English -> Estonian interpretation, interpretation history, and some sort of flashcard memorization mechanic. Stay tuned!" ]
        ]


tryDeepTranslateButton : Element FrontendMsg
tryDeepTranslateButton =
    Element.el
        [ Border.width 2
        , Border.color <| Element.rgb 1 0 0
        ]
    <|
        Element.text "tryDeepTranslateButton"


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
