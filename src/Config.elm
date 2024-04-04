module Config exposing (..)


type alias PublicUsageConfig =
    { addCreditIntervalMillis : Float
    , addCreditAmount : Int
    , maxCapacity : Int
    }


publicUsageConfig : PublicUsageConfig
publicUsageConfig =
    { addCreditIntervalMillis = 1000 * 60 * 5 -- 5 min
    , addCreditAmount = 5
    , maxCapacity = 100
    }


newFeaturesConsentWording : String
newFeaturesConsentWording =
    "hearing about major new features for Eestisse when they come out"


userInterviewsConsentWording : String
userInterviewsConsentWording =
    "participating in a 30-minute user interview in exhange for free credit"
