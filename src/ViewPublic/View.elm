module ViewPublic.View exposing (..)

import Colors
import CommonTypes exposing (..)
import CommonView exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Events as Events
import Responsive exposing (..)
import Types exposing (..)
import ViewPublic.Types exposing (..)


page : DisplayProfile -> ViewPublicModel -> Element FrontendMsg
page dProfile model =
    primaryBox
        [ Element.width Element.fill
        ]
        (maybeViewTranslationList dProfile model.fetchedTranslations)


maybeViewTranslationList : DisplayProfile -> Maybe (List ( Int, TranslationRecord )) -> Element FrontendMsg
maybeViewTranslationList dProfile maybeTranslationRecords =
    case maybeTranslationRecords of
        Nothing ->
            Element.text "loading..."

        Just translationRecords ->
            Element.column
                [ Element.spacing 20
                , Element.width Element.fill
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
        , Element.Background.color <| Colors.offWhite
        , Element.pointer
        , Events.onClick NoOpFrontendMsg
        ]
        [ textWithCutoff translationRecord.input
        , hbreakElement
        , textWithCutoff translationRecord.translation.translation
        ]
