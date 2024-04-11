module RPC exposing (..)

import Env
import Http
import Json.Decode
import Json.Encode
import Lamdera exposing (SessionId)
import LamderaRPC exposing (RPC(..))
import Stripe
import Types exposing (..)


lamdera_handleEndpoints :
    LamderaRPC.RPCArgs
    -> BackendModel
    -> ( LamderaRPC.RPCResult, BackendModel, Cmd BackendMsg )
lamdera_handleEndpoints args model =
    case args.endpoint of
        "stripe" ->
            LamderaRPC.handleEndpointJson purchaseCompletedEndpoint args model

        _ ->
            ( LamderaRPC.ResultFailure <| Http.BadBody <| "Unknown endpoint " ++ args.endpoint, model, Cmd.none )


purchaseCompletedEndpoint :
    SessionId
    -> BackendModel
    -> Json.Decode.Value
    -> ( Result Http.Error Json.Decode.Value, BackendModel, Cmd BackendMsg )
purchaseCompletedEndpoint _ model request =
    let
        response =
            case Env.mode of
                Env.Development ->
                    Ok (Json.Encode.string "prod")

                Env.Production ->
                    Ok (Json.Encode.string "dev")
    in
    case Json.Decode.decodeValue Stripe.decodeWebhook request of
        Ok webhook ->
            case webhook of
                Stripe.StripeSessionCompleted stripeSessionData ->
                    let
                        userEmail =
                            stripeSessionData.clientReferenceId

                        newModel =
                            wut

                        -- initiate paidUser acct (or reactivate archived)
                        cmd =
                            wut

                        -- send to clients (found by session) of this user a msg for successful payment
                    in
                    ( response
                    , newModel
                    , cmd
                    )

        Err error ->
            let
                errorText =
                    "Failed to decode webhook: "
                        ++ Json.Decode.errorToString error
            in
            ( Err (Http.BadBody errorText), model, Backend.errorEmail errorText )
