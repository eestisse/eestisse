module Evergreen.V45.EmailCode exposing (..)

import Time


type alias PendingEmailAuth =
    { email : String
    , expires : Time.Posix
    }
