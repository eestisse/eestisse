module RPC exposing (..)

import Backend
import Dict
import Env
import Http
import Json.Decode
import Json.Encode
import Lamdera exposing (SessionId)
import Lamdera.Json
import LamderaRPC exposing (RPC(..))
import Stripe
import Types exposing (..)


lamdera_handleEndpoints :
    Lamdera.Json.Value
    -> LamderaRPC.HttpRequest
    -> BackendModel
    -> ( LamderaRPC.RPCResult, BackendModel, Cmd BackendMsg )
lamdera_handleEndpoints _ req model =
    case req.endpoint of
        "stripe" ->
            LamderaRPC.handleEndpointJson stripeWebhookHandler req model

        _ ->
            ( LamderaRPC.resultWith LamderaRPC.StatusNotFound [] <| LamderaRPC.BodyString <| "Unknown endpoint " ++ req.endpoint, model, Cmd.none )


stripeWebhookHandler :
    SessionId
    -> BackendModel
    -> LamderaRPC.Headers
    -> Lamdera.Json.Value
    -> ( Result Http.Error Json.Decode.Value, BackendModel, Cmd BackendMsg )
stripeWebhookHandler _ model headers json =
    case ( Json.Decode.decodeValue Stripe.decodeWebhook json, checkStripeHeaderSignature headers ) of
        ( _, False ) ->
            ( Err (Http.BadBody "invalid stripe signature"), model, Backend.notifyAdminOfError "invalid stripe signature" )

        ( Err error, _ ) ->
            let
                errorText =
                    "Failed to decode webhook: "
                        ++ Json.Decode.errorToString error
            in
            ( Err (Http.BadBody errorText), model, Cmd.none )

        ( Ok webhook, True ) ->
            Backend.handleStripeWebhook webhook model


checkStripeHeaderSignature : LamderaRPC.Headers -> Bool
checkStripeHeaderSignature args =
    Debug.todo ""
