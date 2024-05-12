module Testing exposing (..)

import Time
import Translation.Types exposing (..)
import Types exposing (..)


completedRequestExample : CompletedRequest
completedRequestExample =
    { inputText = ""
    , translationResult =
        Ok <|
            translation
    , maybeSelectedBreakdownPart = Nothing
    }


translation : Translation
translation =
    { translatedTo = English
    , translatedText = "my name is Logan"
    , breakdown =
        [ { estonian = "minu"
          , englishTranslation = "my"
          , maybeExplanation = Nothing
          }
        , { estonian = "nimi"
          , englishTranslation = "is"
          , maybeExplanation = Just "some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here "
          }
        , { estonian = "on"
          , englishTranslation = "is"
          , maybeExplanation = Just "some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here "
          }
        , { estonian = "Logan"
          , englishTranslation = "is"
          , maybeExplanation = Just "some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here "
          }
        ]
    }


translationWithIdInText : Int -> Translation
translationWithIdInText id =
    { translation | translatedText = String.fromInt id ++ translation.translatedText }


publicTranslationRecordWithId : Int -> TranslationRecord
publicTranslationRecordWithId id =
    { id = id
    , fromUserId = Nothing
    , public = True
    , time = Time.millisToPosix (id * 1000 * 10)
    , input = String.fromInt id ++ " minu nimi on Logan"
    , translation = translationWithIdInText id
    }
