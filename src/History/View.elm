module History.View exposing (..)

import CommonView exposing (..)
import Dict exposing (Dict)
import Element exposing (Element)
import Responsive exposing (..)
import Translation.Types exposing (..)
import Types exposing (..)
import ViewTranslationList exposing (viewTranslationList)


page : DisplayProfile -> Dict Int TranslationRecord -> Element FrontendMsg
page dProfile cachedTranslationRecords =
    let
        personalTranslationRecords =
            cachedTranslationRecords
                |> Dict.filter
                    (\_ record ->
                        not record.public
                    )
    in
    primaryBox
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (viewTranslationList dProfile personalTranslationRecords)
