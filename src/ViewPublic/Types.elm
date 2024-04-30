module ViewPublic.Types exposing (..)

import CommonTypes exposing (..)


type alias ViewPublicModel =
    { fetchedTranslations : Maybe (List ( Int, TranslationRecord )) }
