module EmailCode exposing (..)

import Env
import Sha256
import Time


type alias PendingEmailAuth =
    { email : String
    , expires : Time.Posix
    }


getUniqueId : Time.Posix -> { a | secretCounter : Int } -> ( { a | secretCounter : Int }, String )
getUniqueId time model =
    ( { model | secretCounter = model.secretCounter + 1 }
    , Env.emailCodeSecret
        ++ ":"
        ++ String.fromInt model.secretCounter
        ++ ":"
        ++ String.fromInt (Time.posixToMillis time)
        |> Sha256.sha256
        |> String.left 8
    )
