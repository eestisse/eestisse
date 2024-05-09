module Backend exposing (..)

import Array exposing (Array)
import Array.Extra
import Auth
import Auth.Flow
import Config
import Dict exposing (Dict)
import Dict.Extra
import Env
import GPTRequests
import Http
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Result.Extra
import Set
import Stripe.Types as Stripe
import Stripe.Utils as Stripe
import Task
import Testing
import Time
import Translation.Types exposing (..)
import Types exposing (..)
import Utils


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { nowish = Time.millisToPosix 0
      , publicCreditsInfo =
            { current = 0
            , nextRefresh = Time.millisToPosix 0
            , refreshAmount = Config.publicUsageConfig.addCreditAmount
            }
      , emails_backup = Set.empty
      , emailsWithConsents = []
      , preConsentRequests = []
      , translationRecords = Array.empty
      , pendingAuths = Dict.empty
      , sessions = Dict.empty
      , users = Dict.empty
      , nextUserId = 0
      , hangingInvoices = []
      }
    , Time.now
        |> Task.perform InitialTimeVal
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        InitialTimeVal t ->
            ( { model
                | nowish = t
                , publicCreditsInfo =
                    { current = 0
                    , nextRefresh =
                        Time.posixToMillis t
                            + Config.publicUsageConfig.addCreditIntervalMillis
                            |> Time.millisToPosix
                    , refreshAmount = Config.publicUsageConfig.addCreditAmount
                    }
              }
            , Cmd.none
            )

        UpdateBackendNow time ->
            ( { model | nowish = time }
            , Cmd.none
            )

        OnConnect sessionId clientId ->
            case Dict.get sessionId model.sessions of
                Nothing ->
                    ( { model
                        | sessions =
                            model.sessions
                                |> Dict.insert
                                    sessionId
                                    blankSession
                      }
                    , Cmd.none
                    )

                Just sessionInfo ->
                    let
                        maybeUserIdAndInfo =
                            sessionInfo.maybeAuthedUserId
                                |> Maybe.andThen
                                    (\userId ->
                                        Dict.get userId model.users
                                            |> Maybe.map (Tuple.pair userId)
                                    )
                    in
                    ( model
                    , maybeUserIdAndInfo
                        |> Maybe.map
                            (\( userId, userInfo ) ->
                                Lamdera.sendToFrontend clientId <| AuthSuccess <| toFrontendUserInfo ( userId, userInfo, Auth.userMembershipStatus model.nowish userInfo )
                            )
                        |> Maybe.withDefault Cmd.none
                    )

        AuthBackendMsg authMsg ->
            Auth.Flow.backendUpdate (Auth.backendConfig model) authMsg

        GptResponseReceived ( sessionId, clientId ) publicConsentChecked input fetchResult ->
            let
                translationRecordResult =
                    GPTRequests.processGptResponse fetchResult
                        |> Result.map
                            (TranslationRecord (Array.length model.translationRecords) (sessionIdToMaybeUserId sessionId model) publicConsentChecked model.nowish input)

                newTranslationRecords =
                    case translationRecordResult of
                        Ok newRecord ->
                            model.translationRecords
                                |> Array.push newRecord

                        Err _ ->
                            model.translationRecords

                modelWithResult =
                    { model
                        | translationRecords = newTranslationRecords
                    }

                ( newModel, bcastCmd ) =
                    case fetchResult of
                        Ok _ ->
                            ( modelWithResult, Cmd.none )

                        Err _ ->
                            modelWithResult |> refundOneCreditAndBroadcast
            in
            ( newModel
            , Cmd.batch
                [ Lamdera.sendToFrontend clientId
                    (TranslationResult input <| translationRecordResult)
                , bcastCmd
                ]
            )

        AddPublicCredits ->
            let
                newPublicCredits =
                    min
                        Config.publicUsageConfig.maxCapacity
                        (model.publicCreditsInfo.current + Config.publicUsageConfig.addCreditAmount)

                newPublicCreditsInfo =
                    { current = newPublicCredits
                    , nextRefresh =
                        Time.posixToMillis model.nowish
                            + Config.publicUsageConfig.addCreditIntervalMillis
                            |> Time.millisToPosix
                    , refreshAmount = Config.publicUsageConfig.addCreditAmount
                    }
            in
            ( { model
                | publicCreditsInfo =
                    newPublicCreditsInfo
              }
            , Lamdera.broadcast <| CreditsInfoUpdated newPublicCreditsInfo
            )

        SubscriptionDataReceived result ->
            case result of
                Err httpErr ->
                    ( model, notifyAdminOfError <| "Http error when fetching subscription data: " ++ Utils.httpErrorToString httpErr )

                Ok subscriptionData ->
                    let
                        paidInvoice : PaidInvoice
                        paidInvoice =
                            { customerId = subscriptionData.customerId
                            , paidUntil = subscriptionData.currentPeriodEnd
                            }

                        maybeMatchingUserIdAndStripeInfo =
                            model.users
                                |> Dict.Extra.filterMap
                                    (\_ user ->
                                        user.stripeInfo
                                            |> Maybe.andThen
                                                (\stripeInfo ->
                                                    if stripeInfo.customerId == paidInvoice.customerId then
                                                        Just stripeInfo

                                                    else
                                                        Nothing
                                                )
                                    )
                                |> Dict.toList
                                |> List.head
                    in
                    case maybeMatchingUserIdAndStripeInfo of
                        Just ( userId, stripeInfo ) ->
                            ( { model
                                | users =
                                    model.users
                                        |> Dict.update userId
                                            (Maybe.map (updateUserStripeInfo { stripeInfo | paidUntil = Just paidInvoice.paidUntil }))
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model
                                | hangingInvoices =
                                    model.hangingInvoices
                                        |> List.append [ paidInvoice ]
                              }
                            , Cmd.none
                            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        maybeUserId =
            sessionIdToMaybeUserId sessionId model
    in
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        AuthToBackend authToBackend ->
            Auth.Flow.updateFromFrontend (Auth.backendConfig model) clientId sessionId authToBackend model

        SubmitTextForTranslation publicConsentChecked input ->
            if model.publicCreditsInfo.current > 0 then
                model
                    |> deductOneCreditAndBroadcast
                    |> Tuple.mapSecond
                        (\bcastCmd ->
                            Cmd.batch [ bcastCmd, requestGptTranslationCmd ( sessionId, clientId ) publicConsentChecked input ]
                        )

            else
                ( model
                , Lamdera.sendToFrontend clientId <| TranslationResult input (Err OutOfCredits)
                )

        RequestImportantNumber ->
            ( model
            , Lamdera.sendToFrontend clientId <|
                AdminDataMsg <|
                    { emailsAndConsents =
                        model.emailsWithConsents
                            |> List.map
                                (\emailWithConsents ->
                                    emailWithConsents.consentsGiven
                                        |> List.map (\consent -> ( emailWithConsents.email, consent ))
                                )
                            |> List.concat
                            |> List.Extra.unique
                            |> List.map Tuple.second
                            |> List.Extra.frequencies
                    }
            )

        RequestGeneralData ->
            ( model
            , Lamdera.sendToFrontend clientId <|
                GeneralDataMsg <|
                    GeneralData model.publicCreditsInfo
            )

        DoLogout ->
            Auth.logout sessionId clientId model

        RequestPublicTranslations ->
            ( model
            , Lamdera.sendToFrontend clientId <|
                RequestTranslationRecordsResult <|
                    Ok <|
                        (model.translationRecords
                            |> Array.filter (\tr -> tr.public)
                            |> Array.Extra.reverse
                            |> Array.Extra.sliceUntil 20
                            |> Array.toList
                        )
            )

        RequestTranslation id ->
            case Array.get id model.translationRecords of
                Nothing ->
                    ( model, Cmd.none )

                Just translationRecord ->
                    let
                        hasPermission =
                            translationRecord.public || (maybeUserId == translationRecord.fromUserId)
                    in
                    ( model
                    , if hasPermission then
                        Lamdera.sendToFrontend clientId <|
                            RequestTranslationRecordsResult <|
                                Ok <|
                                    List.singleton <|
                                        translationRecord

                      else
                        Lamdera.sendToFrontend clientId <| RequestTranslationRecordsResult <| Err "You don't have permission to view that translation"
                    )

        SetRedirectReturnPage route ->
            ( { model
                | sessions =
                    model.sessions
                        |> Dict.update sessionId
                            (Maybe.map
                                (\session ->
                                    { session
                                        | redirectReturnPage = Just route
                                    }
                                )
                            )
              }
            , Cmd.none
            )

        RequestAndClearRedirectReturnPage ->
            ( model |> clearRedirectReturnPageForSession sessionId
            , Lamdera.sendToFrontend clientId <|
                RequestRedirectReturnPageResult
                    (model.sessions
                        |> Dict.get sessionId
                        |> Maybe.andThen .redirectReturnPage
                    )
            )


handleStripeWebhook : Stripe.StripeEvent -> BackendModel -> ( Result Http.Error String, BackendModel, Cmd BackendMsg )
handleStripeWebhook webhook model =
    let
        okResponse =
            case Env.mode of
                Env.Development ->
                    Ok "prod"

                Env.Production ->
                    Ok "dev"
    in
    case webhook of
        Stripe.CheckoutSessionCompleted stripeSessionData ->
            case ( stripeSessionData.clientReferenceId, stripeSessionData.subscriptionId, stripeSessionData.customerId ) of
                ( Nothing, _, _ ) ->
                    ( okResponse, model, notifyAdminOfError <| "no clientReferenceId in StripeSessionCompleted. Session ID: " ++ stripeSessionData.id )

                ( _, Nothing, _ ) ->
                    ( okResponse, model, notifyAdminOfError <| "no subscriptionId in StripeSessionCompleted. Session ID: " ++ stripeSessionData.id )

                ( _, _, Nothing ) ->
                    ( okResponse, model, notifyAdminOfError <| "no customerId in StripeSessionCompleted. Session ID: " ++ stripeSessionData.id )

                ( Just userId, Just subscriptionId, Just customerId ) ->
                    let
                        -- if there is a matched invoice, we want to remember it and remove it from hangingInvoices
                        ( newHangingInvoices, maybeMatchedInvoice ) =
                            case List.Extra.findIndex (.customerId >> (==) customerId) model.hangingInvoices of
                                Just i ->
                                    ( List.Extra.removeAt i model.hangingInvoices, List.Extra.getAt i model.hangingInvoices )

                                Nothing ->
                                    ( model.hangingInvoices, Nothing )

                        stripeInfo =
                            { customerId = customerId
                            , subscriptionId = subscriptionId
                            , paidUntil = maybeMatchedInvoice |> Maybe.map .paidUntil
                            }
                    in
                    ( okResponse
                    , { model
                        | hangingInvoices = newHangingInvoices
                        , users =
                            model.users
                                |> Dict.update userId
                                    (Maybe.map
                                        (\userInfo ->
                                            { userInfo | stripeInfo = Just stripeInfo }
                                        )
                                    )
                      }
                    , Cmd.none
                    )

        Stripe.InvoicePaid invoiceData ->
            case ( invoiceData.customerId, invoiceData.subscriptionId ) of
                ( Nothing, _ ) ->
                    ( okResponse, model, notifyAdminOfError <| "no customerId in InvoicePaid. Session ID: " ++ invoiceData.id )

                ( _, Nothing ) ->
                    ( okResponse, model, notifyAdminOfError <| "no subscrptionId in InvoicePaid. Session ID: " ++ invoiceData.id )

                ( Just _, Just subscriptionId ) ->
                    ( okResponse
                    , model
                    , Stripe.getSubscriptionData subscriptionId SubscriptionDataReceived
                    )


signupFormToEmailAndConsets : SignupFormModel -> EmailAndConsents
signupFormToEmailAndConsets signupForm =
    { email = signupForm.emailInput
    , consentsGiven =
        List.concat
            [ if signupForm.newFeaturesConsentChecked then
                [ Config.newFeaturesConsentWording ]

              else
                []
            , if signupForm.userInterviewsConsentChecked then
                [ Config.userInterviewsConsentWording ]

              else
                []
            ]
    }



-- requestClaudeTranslationCmd : ClientId -> String -> Cmd BackendMsg
-- requestClaudeTranslationCmd clientId inputText =
--     Http.request
--         { method = "POST"
--         , headers =
--             [ Http.header "x-api-key" Env.anthropicApiKey
--             , Http.header "anthropic-version" "2023-06-01"
--             ]
--         , url = "https://api.anthropic.com/v1/messages"
--         , body = Http.jsonBody <| ClaudeRequests.encode <| ClaudeRequests.translateFromEstonian inputText
--         , expect = Http.expectJson (GptResponseReceived clientId inputText) ClaudeRequests.apiResponseDecoder
--         , timeout = Nothing
--         , tracker = Nothing
--         }


requestGptTranslationCmd : ( SessionId, ClientId ) -> Bool -> String -> Cmd BackendMsg
requestGptTranslationCmd sessionAndClientId publicConsentChecked inputText =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Env.openaiApiKey) ]
        , url = "https://api.openai.com/v1/chat/completions"
        , body = Http.jsonBody <| GPTRequests.encode <| GPTRequests.translateFromEstonian inputText
        , expect = Http.expectJson (GptResponseReceived sessionAndClientId publicConsentChecked inputText) GPTRequests.apiResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deductOneCreditAndBroadcast : BackendModel -> ( BackendModel, Cmd BackendMsg )
deductOneCreditAndBroadcast model =
    modifyCreditBalanceAndBroadcast (model.publicCreditsInfo.current - 1) model


refundOneCreditAndBroadcast : BackendModel -> ( BackendModel, Cmd BackendMsg )
refundOneCreditAndBroadcast model =
    modifyCreditBalanceAndBroadcast (model.publicCreditsInfo.current + 1) model


modifyCreditBalanceAndBroadcast : Int -> BackendModel -> ( BackendModel, Cmd BackendMsg )
modifyCreditBalanceAndBroadcast newCredits model =
    let
        newPublicCreditsInfo =
            let
                old =
                    model.publicCreditsInfo
            in
            { old
                | current = newCredits
            }
    in
    ( { model
        | publicCreditsInfo = newPublicCreditsInfo
      }
    , Lamdera.broadcast <| CreditsInfoUpdated newPublicCreditsInfo
    )


notifyAdminOfError : String -> Cmd BackendMsg
notifyAdminOfError s =
    let
        _ =
            Debug.log "hey admin! Error: " s
    in
    Cmd.none


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Time.every (toFloat Config.publicUsageConfig.addCreditIntervalMillis) (always AddPublicCredits)
        , Time.every 1000 UpdateBackendNow
        , Lamdera.onConnect OnConnect
        ]


sessionIdToMaybeUserId : SessionId -> BackendModel -> Maybe Int
sessionIdToMaybeUserId sessionId model =
    model.sessions
        |> Dict.get sessionId
        |> Maybe.andThen .maybeAuthedUserId


clearRedirectReturnPageForSession : SessionId -> BackendModel -> BackendModel
clearRedirectReturnPageForSession sessionId model =
    { model
        | sessions =
            model.sessions
                |> Dict.update sessionId
                    (Maybe.map (\session -> { session | redirectReturnPage = Nothing }))
    }
