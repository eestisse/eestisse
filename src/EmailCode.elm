module EmailCode exposing (..)

import Config
import Email.Html as Html
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
    Postmark.BodyHtml <|
        Html.div []
            [ Html.p [] [ Html.text "Your login code is:" ]
            , Html.h2 [] [ Html.text code ]
            , Html.p [] [ Html.text <| "This code will expire in " ++ Config.emailCodeExpirationString ++ "." ]
            , Html.p [] [ Html.text <| "If you are not trying to log in to eestisse.ee, you can safely ignore this email." ]
            ]
