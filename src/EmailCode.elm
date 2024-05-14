module EmailCode exposing (..)

import Config
import Env
import Postmark exposing (PostmarkEmailBody)
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


buildEmailBody : String -> PostmarkEmailBody
buildEmailBody code =
    Postmark.BodyText <|
        "Your login code is:\n\n"
            ++ code
            ++ "\n\nThis code will exprire in "
            ++ Config.emailCodeExpirationString
            ++ ".\n\nIf you are not trying to log in to eestisse.ee, you can safely ignore this email."
