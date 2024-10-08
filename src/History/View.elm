module History.View exposing (..)

import CommonView exposing (..)
import Dict exposing (Dict)
import Element exposing (Element)
import Responsive exposing (..)
import Translation.Types exposing (..)
import Types exposing (..)
import ViewTranslationList exposing (..)


page : DisplayProfile -> Int -> Dict Int TranslationRecord -> FetchButtonVisibility -> Element FrontendMsg
page dProfile userId cachedTranslationRecords fetchButtonVisibility =
    let
        personalTranslationRecords =
            cachedTranslationRecords
                |> Dict.filter
                    (\_ record ->
                        record.fromUserId == Just userId
                    )
    in
    primaryBox
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (viewTranslationList dProfile personalTranslationRecords Personal fetchButtonVisibility)
