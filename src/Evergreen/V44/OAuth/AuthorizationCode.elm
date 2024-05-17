module Evergreen.V44.OAuth.AuthorizationCode exposing (..)

import Evergreen.V44.OAuth


type alias AuthorizationError =
    { error : Evergreen.V44.OAuth.ErrorCode
    , errorDescription : Maybe String
    , errorUri : Maybe String
    , state : Maybe String
    }


type alias AuthenticationError =
    { error : Evergreen.V44.OAuth.ErrorCode
    , errorDescription : Maybe String
    , errorUri : Maybe String
    }
