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
    , maxCapacity = 30000
    }
