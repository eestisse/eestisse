module Config exposing (..)

import EmailAddress exposing (EmailAddress)
import Env
import Time.Extra


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
    "a 30-minute user interview call in exchange for free credit"


stripePaymentLinkBaseUrl : String
stripePaymentLinkBaseUrl =
    "https://buy.stripe.com"


stripePaymentLinkId : String
stripePaymentLinkId =
    if Env.mode == Env.Development then
        "test_dR66sbe114VB69G7st"

    else
        "test_dR66sbe114VB69G7st"


stripeUserPortalLink : String
stripeUserPortalLink =
    if Env.mode == Env.Development then
        "https://billing.stripe.com/p/login/test_dR66pse1idd55cAdQQ"

    else
        "https://billing.stripe.com/p/login/test_dR66pse1idd55cAdQQ"


maxNumRecordsResponse : Int
maxNumRecordsResponse =
    100


frontendFetchRecordCount : Int
frontendFetchRecordCount =
    25


emailCodeExpirationInfo =
    ( 1000 * 60 * 5, "5 minutes" )


emailCodeExpirationMillis : Int
emailCodeExpirationMillis =
    Tuple.first emailCodeExpirationInfo


emailCodeExpirationString : String
emailCodeExpirationString =
    Tuple.second emailCodeExpirationInfo


loginCodeFromEmail : EmailAddress
loginCodeFromEmail =
    { localPart = "login"
    , domain = "eestisse"
    , tags = []
    , tld = [ "ee" ]
    }


serverEmail : EmailAddress
serverEmail =
    { localPart = "server"
    , domain = "eestisse"
    , tags = []
    , tld = [ "ee" ]
    }


adminEmail : EmailAddress
adminEmail =
    { localPart = "admin"
    , domain = "eestisse"
    , tags = []
    , tld = [ "ee" ]
    }


intervalWaitBetweenAdminErrorEmails : ( Time.Extra.Interval, Int )
intervalWaitBetweenAdminErrorEmails =
    ( Time.Extra.Hour, 3 )
