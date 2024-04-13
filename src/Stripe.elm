module Stripe exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (..)
import Time


type Webhook
    = CheckoutSessionCompleted Session
    | InvoicePaid Invoice


type alias Session =
    { id : String
    , clientReferenceId : Maybe String
    , customerId : String
    , subscriptionId : String
    }


type alias Invoice =
    { id : String
    , subscriptionId : String
    }


decodeWebhook : D.Decoder Webhook
decodeWebhook =
    D.field "type" D.string
        |> D.andThen
            (\eventType ->
                case eventType of
                    "checkout.session.completed" ->
                        D.map
                            CheckoutSessionCompleted
                            (D.succeed Session
                                |> required "data" (D.field "object" (D.field "id" D.string))
                                |> required "data" (D.field "object" (D.nullable <| D.field "client_reference_id" D.string))
                                |> required "data" (D.field "object" (D.field "customer" D.string))
                                |> required "data" (D.field "object" (D.field "subscription" D.string))
                            )

                    "invoice.paid" ->
                        D.map
                            InvoicePaid
                            (D.succeed Invoice
                                |> required "data" (D.field "object" (D.field "id" D.string))
                                |> required "data" (D.field "object" (D.field "subscription" D.string))
                            )

                    _ ->
                        D.fail ("Unhandled stripe webhook event: " ++ eventType)
            )
