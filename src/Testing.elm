module Testing exposing (..)

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
