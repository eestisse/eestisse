module Evergreen.V46.EmailAddress exposing (..)


type alias EmailAddress =
    { localPart : String
    , tags : List String
    , domain : String
    , tld : List String
    }
