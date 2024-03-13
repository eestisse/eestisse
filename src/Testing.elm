module Testing exposing (..)

import Types exposing (..)


completedRequestExample : CompletedRequest
completedRequestExample =
    { inputText = ""
    , translationResult =
        Ok <|
            { breakdown =
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
                  , maybeExplanation = Just "some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here "
                  }
                ]
            , translation = "my name is Logan"
            }
    , maybeSelectedBreakdownPart = Nothing
    }


completedRequestExample2 : CompletedRequest
completedRequestExample2 =
    { inputText = ""
    , translationResult =
        Ok <|
            { breakdown =
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
                , { estonian = "Logan Logan Logan Logan Logan Logan Logan Logan Logan Logan Logan Logan Logan Logan Logan Logan Logan Logan Logan "
                  , englishTranslation = "is is is is is is is is is is is is is is is is is is is is is is is is is is is is is is is is is is is is "
                  , maybeExplanation = Just "some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here "
                  }
                ]
            , translation = "my name is Logan"
            }
    , maybeSelectedBreakdownPart = Nothing
    }
