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
import Time
import Types exposing (..)


lamdera_handleEndpoints :
    Lamdera.Json.Value
    -> LamderaRPC.HttpRequest
    -> BackendModel
    -> ( LamderaRPC.RPCResult, BackendModel, Cmd BackendMsg )
lamdera_handleEndpoints _ req model =
    case req.endpoint of
        "stripe" ->
            LamderaRPC.handleEndpointString stripeWebhookHandler req model

        _ ->
            ( LamderaRPC.resultWith LamderaRPC.StatusNotFound [] <| LamderaRPC.BodyString <| "Unknown endpoint " ++ req.endpoint, model, Cmd.none )


stripeWebhookHandler :
    SessionId
    -> BackendModel
    -> LamderaRPC.Headers
    -> String
    -> ( Result Http.Error String, BackendModel, Cmd BackendMsg )
stripeWebhookHandler _ model headers bodyString =
    case ( Json.Decode.decodeString Stripe.decodeWebhook bodyString, checkStripeHeaderSignature bodyString headers Env.stripeWebhookSecret model.nowish ) of
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


checkStripeHeaderSignature : String -> LamderaRPC.Headers -> String -> Time.Posix -> Bool
checkStripeHeaderSignature bodyString headers endpointSecret receivedTimestamp =
    let
        boolResult =
            headers
                |> Dict.get "stripe-signature"
                |> Result.fromMaybe "signature header not found"
                |> Result.andThen (Stripe.signaturePayloadToSigStruct >> Result.fromMaybe "signature payload could not be decoded")
                |> Result.map
                    (\sigStruct ->
                        Stripe.signatureIsValid bodyString sigStruct endpointSecret receivedTimestamp
                    )
    in
    case boolResult of
        Err s ->
            let
                _ =
                    Debug.log "error checking stripe signature:" s
            in
            False

        Ok False ->
            let
                _ =
                    Debug.log "signature invalid" ""
            in
            False

        Ok True ->
            let
                _ =
                    Debug.log "sig right!" ""
            in
            True
