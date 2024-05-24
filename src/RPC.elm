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
import Stripe.Types as Stripe
import Stripe.Utils as Stripe
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
            LamderaRPC.handleEndpointJson stripeWebhookHandler req model

        _ ->
            ( LamderaRPC.resultWith LamderaRPC.StatusNotFound [] <| LamderaRPC.BodyString <| "Unknown endpoint " ++ req.endpoint, model, Cmd.none )


stripeWebhookHandler :
    SessionId
    -> BackendModel
    -> LamderaRPC.Headers
    -> Lamdera.Json.Value
    -> ( Result Http.Error Lamdera.Json.Value, BackendModel, Cmd BackendMsg )
stripeWebhookHandler _ model headers bodyJson =
    ( Err <| Http.BadBody "wow stupid error", model, Cmd.none )



-- case Json.Decode.decodeValue Stripe.stripeEventDecoder bodyJson of
--     Err error ->
--         let
--             errorText =
--                 "Failed to decode stripe webhook: "
--                     ++ Json.Decode.errorToString error
--         in
--         model
--             |> Backend.notifyAdminOfError errorText
--             |> (\( m, c ) ->
--                     ( Err (Http.BadBody errorText), m, c )
--                )
--     Ok event ->
--         Backend.handleStripeWebhook event model
-- oldStripeWebhookHandler :
--     SessionId
--     -> BackendModel
--     -> LamderaRPC.Headers
--     -> String
--     -> ( Result Http.Error String, BackendModel, Cmd BackendMsg )
-- oldStripeWebhookHandler _ model headers bodyString =
--     case ( Json.Decode.decodeString Stripe.stripeEventDecoder bodyString, checkStripeHeaderSignature bodyString headers Env.stripeWebhookSecret model.time_bySecond ) of
--         ( _, False ) ->
--             (model |> Backend.notifyAdminOfError "invalid stripe signature")
--                 |> (\( m, c ) -> ( Err (Http.BadBody "invalid stripe signature"), m, c ))
--         ( Err error, _ ) ->
--             let
--                 errorText =
--                     "Failed to decode stripe webhook: "
--                         ++ Json.Decode.errorToString error
--             in
--             model
--                 |> Backend.notifyAdminOfError errorText
--                 |> (\( m, c ) ->
--                         ( Err (Http.BadBody errorText), m, c )
--                    )
--         ( Ok webhook, True ) ->
--             Backend.handleStripeWebhook webhook model


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
            False

        Ok False ->
            False

        Ok True ->
            True
