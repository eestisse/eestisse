module Stripe.Types exposing (..)

import Time


type StripeEvent
    = CheckoutSessionCompleted CheckoutSession
    | InvoicePaid Invoice


type alias CheckoutSession =
    { id : String
    , clientReferenceId : Maybe String
    , customerId : Maybe String
    , subscriptionId : Maybe String
    }


type alias Invoice =
    { id : String
    , customerId : Maybe String
    , subscriptionId : Maybe String
    }


type alias SignatureStruct =
    { timestamp : Int
    , v1Signatures : List String
    }


type alias SubscriptionData =
    { id : String
    , customerId : String
    , currentPeriodEnd : Time.Posix
    }
