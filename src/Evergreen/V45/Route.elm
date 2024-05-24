module Evergreen.V45.Route exposing (..)


type Route
    = Landing
    | Translate
    | Admin
    | AuthCallback String
    | Account
    | StripeLinkback
    | Browse
    | History
    | View Int
    | Feedback
    | BadRoute String
