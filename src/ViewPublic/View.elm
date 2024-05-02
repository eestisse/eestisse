module ViewPublic.View exposing (..)

import Colors
import CommonTypes exposing (..)
import CommonView exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Responsive exposing (..)
import Types exposing (..)
import ViewPublic.Types exposing (..)


page : DisplayProfile -> ViewPublicModel -> Element FrontendMsg
page dProfile model =
    primaryBox
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (maybeViewTranslationList dProfile model.fetchedTranslations)


maybeViewTranslationList : DisplayProfile -> Maybe (List ( Int, TranslationRecord )) -> Element FrontendMsg
maybeViewTranslationList dProfile maybeTranslationRecords =
    case maybeTranslationRecords of
        Nothing ->
            Element.text "loading..."

        Just translationRecords ->
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
                        |> List.map (viewTranslationRecordPreviewButton dProfile)
                    )


viewTranslationRecordPreviewButton : DisplayProfile -> ( Int, TranslationRecord ) -> Element FrontendMsg
viewTranslationRecordPreviewButton dProfile ( id, translationRecord ) =
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
        , Events.onClick NoOpFrontendMsg
        ]
        [ textWithCutoff translationRecord.input
        , hbreakElement
        , textWithCutoff translationRecord.translation.translation
        ]
