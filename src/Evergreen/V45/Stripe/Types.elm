module Evergreen.V45.Stripe.Types exposing (..)

import Time


type alias SubscriptionData =
    { id : String
    , customerId : String
    , currentPeriodEnd : Time.Posix
    }
