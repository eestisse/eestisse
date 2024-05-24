module Evergreen.V46.OAuth.AuthorizationCode exposing (..)

import Evergreen.V46.OAuth


type alias AuthorizationError =
    { error : Evergreen.V46.OAuth.ErrorCode
    , errorDescription : Maybe String
    , errorUri : Maybe String
    , state : Maybe String
    }


type alias AuthenticationError =
    { error : Evergreen.V46.OAuth.ErrorCode
    , errorDescription : Maybe String
    , errorUri : Maybe String
    }
