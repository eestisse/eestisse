module Stripe exposing (..)

import Crypto.HMAC
import Dict
import Json.Decode as D
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Maybe.Extra
import Time
import Utils


type Webhook
    = CheckoutSessionCompleted CheckoutSession
    | InvoicePaid Invoice


type alias CheckoutSession =
    { id : String
    , clientReferenceId : Maybe String
    , customerId : Maybe String
    , subscriptionId : Maybe String
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
                            (D.succeed CheckoutSession
                                |> required "data" (D.field "object" (D.field "id" D.string))
                                |> required "data" (D.field "object" (D.field "client_reference_id" <| D.nullable D.string))
                                |> required "data" (D.field "object" (D.field "customer" <| D.nullable D.string))
                                |> required "data" (D.field "object" (D.field "subscription" <| D.nullable D.string))
                            )

                    -- "invoice.paid" ->
                    --     D.map
                    --         InvoicePaid
                    --         (D.succeed Invoice
                    --             |> required "data" (D.field "object" (D.field "id" D.string))
                    --             |> required "data" (D.field "object" (D.field "subscription" D.string))
                    --         )
                    _ ->
                        D.fail ("Unhandled stripe webhook event: " ++ eventType)
            )


type alias SignatureStruct =
    { timestamp : Int
    , v1Signatures : List String
    }


signaturePayloadToSigStruct : String -> Maybe SignatureStruct
signaturePayloadToSigStruct payload =
    let
        valsListToSignature : List ( String, String ) -> Maybe SignatureStruct
        valsListToSignature vals =
            let
                v1Sigs =
                    vals
                        |> List.filterMap
                            (\( k, v ) ->
                                if k == "v1" then
                                    Just v

                                else
                                    Nothing
                            )

                maybeTimestamp =
                    vals
                        |> List.filterMap
                            (\( k, v ) ->
                                if k == "t" then
                                    Just v

                                else
                                    Nothing
                            )
                        |> List.head
                        |> Maybe.andThen String.toInt
            in
            case ( List.length v1Sigs > 0, maybeTimestamp ) of
                ( True, Just timestamp ) ->
                    Just
                        { timestamp = timestamp
                        , v1Signatures = v1Sigs
                        }

                _ ->
                    Nothing
    in
    payload
        |> String.split ","
        |> List.map (String.split "=")
        |> List.map Utils.list2ToTuple
        |> Maybe.Extra.combine
        |> Maybe.andThen valsListToSignature


signatureIsValid : String -> SignatureStruct -> String -> Time.Posix -> Bool
signatureIsValid rawReq sigStruct endpointSecret receivedTimestamp =
    let
        signed_payload =
            String.fromInt sigStruct.timestamp ++ "." ++ rawReq

        expectedSig =
            Crypto.HMAC.digest Crypto.HMAC.sha256 endpointSecret signed_payload

        validSigFound =
            sigStruct.v1Signatures
                |> List.any ((==) expectedSig)

        receivedUnixTime =
            receivedTimestamp
                |> Time.posixToMillis
                |> (\i -> i // 1000)

        timeDiffSeconds =
            abs (receivedUnixTime - sigStruct.timestamp)
    in
    timeDiffSeconds < 60 && validSigFound
