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
        [ mainExplainer
        , futureFeaturesAndSignupElement
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
        [ Element.width Element.fill
        , Element.spacing 20
        , Element.padding 10
        , Border.width 1
        , Background.color <| Element.rgb 0.95 0.95 1
        , Border.color <| Element.rgb 0.8 0.8 1
        , Border.rounded 6
        ]
        [ CommonView.makeParagraphs
            []
            [ [ CommonView.coloredEestisseText []
              , Element.text " (meaning \"into Estonia\") is a tutoring and assistance tool for anyone learning the Estonian language. "
              ]
            , [ emphasizedText "Deep translation"
              , Element.text " is the central feature: Estonian is not just translated, but explained piece by piece, with the help of AI."
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
            [ [ Element.text "As you navigate Estonia, "
              , CommonView.coloredEestisseText []
              , Element.text " can be your personal tutor, translating and explaining anything you see. "
              , Element.text "Ads, bills, news articles, and Tinder messages all work well."
              ]
            , [ Element.text "Or if you really want to learn fast, pick up an Estonian children's book! Trust me. Just tell them it's for your child... your "
              , italics "inner"
              , Element.text " child."
              ]
            , [ Element.text "Before long you'll find you're picking up common words and getting a feel for the grammar and the case system (of which \"eestisse\" is an example, by the way!)"
              ]
            ]
        ]


futureFeaturesAndSignupElement : Element FrontendMsg
futureFeaturesAndSignupElement =
    Element.column
        [ Element.width Element.fill
        , Border.width 1
        , Background.color <| Element.rgb 1 1 0.9
        , Border.color <| Element.rgb 0.7 0.7 0
        , Border.rounded 6
        , Element.spacing 20
        , Element.padding 10
        ]
        [ CommonView.makeParagraphs
            []
            [ [ Element.text "This is just the beginning of "
              , CommonView.coloredEestisseText []
              , Element.text "! Sign up to hear about new features as they pop: English -> Estonian deep translations, creating flash cards from translations, pictures as input, and more!"
              ]
            ]
        , Element.el
            [ Border.width 2
            , Border.color <| Element.rgb 1 0 0
            , Element.centerX
            ]
          <|
            Element.text "emailSignupElement"
        ]
