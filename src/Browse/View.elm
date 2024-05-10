module Browse.View exposing (..)

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
        -- prevents the user seeing their personal histories in the public list, which would imply that their translations are in the *global* public list
        publicTranslationRecords =
            cachedTranslationRecords
                |> Dict.filter
                    (\_ record ->
                        record.public
                    )
    in
    primaryBox
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (viewTranslationList dProfile publicTranslationRecords)
