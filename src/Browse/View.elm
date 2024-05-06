module Browse.View exposing (..)

import Colors
import CommonView exposing (..)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Responsive exposing (..)
import Route
import Translation.Types exposing (..)
import Types exposing (..)


page : DisplayProfile -> Dict Int TranslationRecord -> Element FrontendMsg
page dProfile translationRecords =
    primaryBox
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (maybeViewTranslationList dProfile translationRecords)


maybeViewTranslationList : DisplayProfile -> Dict Int TranslationRecord -> Element FrontendMsg
maybeViewTranslationList dProfile translationRecords =
    scrollbarYEl
        [ Element.padding 5
        , Border.width 1
        , Border.color <| Element.rgba 0 0 0.5 0.4
        , Border.rounded 4
        ]
    <|
        Element.column
            [ Element.spacing <| responsiveVal dProfile 15 20
            , Element.width Element.fill
            , Element.height Element.fill
            ]
            (translationRecords
                |> Dict.values
                |> List.sortBy (.id >> negate)
                |> List.map (viewTranslationRecordPreviewButton dProfile)
            )


viewTranslationRecordPreviewButton : DisplayProfile -> TranslationRecord -> Element FrontendMsg
viewTranslationRecordPreviewButton dProfile translationRecord =
    let
        ( inputStyles, translatedStyles ) =
            case translationRecord.translation.translatedTo of
                English ->
                    ( [ Font.italic ], [] )

                Estonian ->
                    ( [], [ Font.italic ] )
    in
    Element.column
        [ Element.padding 10
        , Element.spacing 5
        , Element.width Element.fill
        , Border.rounded 10
        , Border.width 1
        , Border.color <| Element.rgba 0 0 0.5 0.4
        , Element.Background.color <| Element.rgba 0 0 1 0.05
        , Font.size <| responsiveVal dProfile 16 18
        , Element.pointer
        , Events.onClick <| GotoRouteAndAnimate <| Route.View translationRecord.id
        ]
        [ Element.el inputStyles <| textWithCutoff translationRecord.input
        , hbreakElement
        , Element.el translatedStyles <| textWithCutoff translationRecord.translation.translatedText
        ]
