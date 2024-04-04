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
    "hear about major new features for Eestisse when they come out"


userInterviewsConsentWording : String
userInterviewsConsentWording =
    "receive requests for user interviews (to ensure Eestisse is the best verison it can be!)"
