module About.View exposing (..)

import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Font
import Types exposing (..)


page : Element FrontendMsg
page =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        aboutTextElement


aboutTextElement =
    [ [ Element.text "Eestisse (\"Into Estonia\") is designed to help you learn Estonian as you navigate the real world in Estonia." ]
    , [ Element.text "We recommend using it as you encounter any real Estonian text in your life. This could be simple advertisements, snippets of articles, or parts of a text conversation with a new Estonian friend :) If you don't have an easy source of Estonian handy, children's books are wonderful for practice!" ]
    , [ Element.text "Using the power of AI, Eestisse gives a full interpretation, not just a translation. This includes a breakdown that explains the sentence in parts, and includes information about context, grammar, and nuance that normal translations services ignore." ]
    , [ Element.text "Eestisse is particularly helpful with understanding how the case system works!" ]
    , [ Element.text "Once an interpretation is finished, you can you can tap on parts of the original Estonian text to learn more about that part." ]
    , [ Element.text "In the near future, we will be adding English -> Estonian interpretation, interpretation history, and some sort of flashcard memorization mechanic. Stay tuned!" ]
    ]
        |> List.map
            (Element.paragraph
                [ Element.spacing 2
                , Element.Font.size 18
                ]
            )
        |> Element.column
            [ Element.padding 10
            , Element.Border.width 1
            , Element.Background.color <| Element.rgb 0.95 0.95 1
            , Element.Border.color <| Element.rgb 0.8 0.8 1
            , Element.Border.rounded 6
            , Element.spacing 20
            ]
