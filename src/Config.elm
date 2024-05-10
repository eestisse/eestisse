module Config exposing (..)

import Env


type alias PublicUsageConfig =
    { addCreditIntervalMillis : Int
    , addCreditAmount : Int
    , maxCapacity : Int
    }


counterUpdateAnimationIntervalMillis : Float
counterUpdateAnimationIntervalMillis =
    500


publicUsageConfig : PublicUsageConfig
publicUsageConfig =
    { addCreditIntervalMillis = 1000 * 60 * 5 -- 5 min
    , addCreditAmount = 2
    , maxCapacity = 20
    }


newFeaturesConsentWording : String
newFeaturesConsentWording =
    "hearing about major new features for Eestisse when they come out"


userInterviewsConsentWording : String
userInterviewsConsentWording =
    "an offer of free credit in exchange for a 30-minute user interview"


stripePaymentLinkBaseUrl : String
stripePaymentLinkBaseUrl =
    "https://buy.stripe.com"


stripePaymentLinkId : String
stripePaymentLinkId =
    if Env.mode == Env.Development then
        "test_dR66sbe114VB69G7st"

    else
        Debug.todo "need real payment link id"


stripeUserPortalLink : String
stripeUserPortalLink =
    if Env.mode == Env.Development then
        "https://billing.stripe.com/p/login/test_dR66pse1idd55cAdQQ"

    else
        Debug.todo "need real stripe user portal link"


maxNumRecordsResponse : Int
maxNumRecordsResponse =
    100
