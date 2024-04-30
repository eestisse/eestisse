module ViewPublic.View exposing (..)

import CommonTypes exposing (..)
import CommonView exposing (..)
import Element exposing (Element)
import Element.Input as Input
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
                ]
                (translationRecords
                    |> List.map (viewTranslationRecordPreviewButton dProfile)
                )


viewTranslationRecordPreviewButton : DisplayProfile -> ( Int, TranslationRecord ) -> Element FrontendMsg
viewTranslationRecordPreviewButton dProfile ( id, translationRecord ) =
    Element.text translationRecord.input
