module Stripe exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (..)


type Webhook
    = StripeSessionCompleted Session


type alias Session =
    { id : String
    , clientReferenceId : String
    }


decodeWebhook : D.Decoder Webhook
decodeWebhook =
    D.field "type" D.string
        |> D.andThen
            (\eventType ->
                case eventType of
                    "checkout.session.completed" ->
                        D.map
                            StripeSessionCompleted
                            (D.succeed Session
                                |> required "data" (D.field "object" (D.field "id" D.string))
                                |> required "data" (D.field "object" (D.field "client_reference_id" D.string))
                            )

                    _ ->
                        D.fail ("Unhandled stripe webhook event: " ++ eventType)
            )
