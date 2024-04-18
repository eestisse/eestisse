module Stripe.Utils exposing (..)

import Crypto.HMAC
import Env
import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (..)
import Maybe.Extra
import Stripe.Types exposing (..)
import Time
import Utils


getSubscriptionData : String -> (Result Http.Error SubscriptionData -> msg) -> Cmd msg
getSubscriptionData subscriptionId msgConstructor =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Env.stripeApiKey) ]
        , url = "https://api.stripe.com/v1/subscriptions/" ++ subscriptionId
        , body = Http.emptyBody
        , expect = Http.expectJson msgConstructor subscriptionDataDecoder
        , timeout = Nothing
        , tracker = Nothing
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


stripeEventDecoder : D.Decoder StripeEvent
stripeEventDecoder =
    D.field "type" D.string
        |> D.andThen
            (\eventType ->
                case eventType of
                    "checkout.session.completed" ->
                        D.map
                            CheckoutSessionCompleted
                            (D.succeed CheckoutSession
                                |> required "data" (D.field "object" (D.field "id" D.string))
                                |> required "data" (D.field "object" (D.field "client_reference_id" <| D.nullable userIdDecoder))
                                |> required "data" (D.field "object" (D.field "customer" <| D.nullable D.string))
                                |> required "data" (D.field "object" (D.field "subscription" <| D.nullable D.string))
                            )

                    "invoice.paid" ->
                        D.map
                            InvoicePaid
                            (D.succeed Invoice
                                |> required "data" (D.field "object" (D.field "id" D.string))
                                |> required "data" (D.field "object" (D.field "customer" <| D.nullable D.string))
                                |> required "data" (D.field "object" (D.field "subscription" <| D.nullable D.string))
                            )

                    _ ->
                        D.fail ("Unhandled stripe webhook event: " ++ eventType)
            )


userIdDecoder : D.Decoder Int
userIdDecoder =
    D.string
        |> D.andThen
            (String.toInt
                >> Maybe.map D.succeed
                >> Maybe.withDefault (D.fail "non-inty string")
            )


subscriptionDataDecoder : D.Decoder SubscriptionData
subscriptionDataDecoder =
    D.succeed SubscriptionData
        |> required "id" D.string
        |> required "customer" D.string
        |> required "current_period_end" timestampDecoder


timestampDecoder : D.Decoder Time.Posix
timestampDecoder =
    D.int |> D.map Utils.unixTimestampToPosix
