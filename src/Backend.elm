module Backend exposing (..)

import Array exposing (Array)
import Array.Extra
import Auth
import Auth.Flow
import Config
import Dict exposing (Dict)
import Dict.Extra
import EmailAddress
import EmailCode
import Env
import GPTRequests
import Http
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Postmark exposing (PostmarkEmailBody, PostmarkSend)
import Set
import Stripe.Types as Stripe
import Stripe.Utils as Stripe
import Task
import Testing
import Time
import Time.Extra
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
      , pendingEmailAuths = Dict.empty
      , secretCounter = 0
      , adminMessages = []
      , lastAdminAlertEmailSent = Time.millisToPosix 0
      }
    , Time.now
        |> Task.perform InitialTimeVal
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        Daily ->
            ( model |> clearVeryExpiredLoginCodes
            , Cmd.none
            )

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
                    model |> notifyAdminOfError ("Http error when fetching subscription data: " ++ Utils.httpErrorToString httpErr)

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
                            let
                                newUsers =
                                    model.users
                                        |> Dict.update userId
                                            (Maybe.map (updateUserStripeInfo { stripeInfo | paidUntil = Just paidInvoice.paidUntil }))

                                numUsersWhoHavePaid =
                                    newUsers
                                        |> Dict.filter
                                            (\_ userInfo ->
                                                (userInfo.stripeInfo |> Maybe.andThen .paidUntil) /= Nothing
                                            )
                                        |> Dict.toList
                                        |> List.length
                            in
                            { model
                                | users = newUsers
                            }
                                |> (if Config.numEarlybirdOffersTotal - numUsersWhoHavePaid >= Config.earlybirdOffersLeftAlertThreshold then
                                        notifyAdminOfError <| "Look out! Only " ++ String.fromInt (Config.numEarlybirdOffersTotal - numUsersWhoHavePaid) ++ " earlybird specials left!"

                                    else
                                        \m -> ( m, Cmd.none )
                                   )

                        Nothing ->
                            ( { model
                                | hangingInvoices =
                                    model.hangingInvoices
                                        |> List.append [ paidInvoice ]
                              }
                            , Cmd.none
                            )

        LoginCodeEmailSentResponse ( emailAddress, code ) response ->
            case response of
                Err httpErr ->
                    model
                        |> notifyAdminOfError
                            ("Couldn't send email for code login! email: " ++ EmailAddress.toString emailAddress ++ "; code: " ++ code ++ " - http error: " ++ Utils.httpErrorToString httpErr)

                Ok _ ->
                    ( model
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
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Lamdera.sendToFrontend clientId LogoutAck
                            ]
                    )

        RequestTranslations publicOrPersonal ( maybeLowestCachedId, count ) ->
            let
                filterFunc =
                    case publicOrPersonal of
                        Public ->
                            .public

                        Personal ->
                            case maybeUserId of
                                Nothing ->
                                    always False

                                Just userId ->
                                    \record -> record.fromUserId == Just userId

                limit =
                    min Config.maxNumRecordsResponse count

                ( listToReturn, noMoreRecords ) =
                    let
                        notYetTruncated =
                            model.translationRecords
                                |> Array.filter filterFunc
                                |> Array.Extra.reverse
                                |> (case maybeLowestCachedId of
                                        Just lowestCachedId ->
                                            Array.toList
                                                >> List.Extra.dropWhile
                                                    (\record ->
                                                        record.id >= lowestCachedId
                                                    )
                                                >> Array.fromList

                                        Nothing ->
                                            identity
                                   )
                    in
                    if Array.length notYetTruncated > limit then
                        ( notYetTruncated
                            |> Array.Extra.sliceUntil limit
                            |> Array.toList
                        , False
                        )

                    else
                        ( notYetTruncated
                            |> Array.toList
                        , True
                        )
            in
            ( model
            , Cmd.batch
                [ Lamdera.sendToFrontend clientId <| RequestTranslationRecordsResult <| Ok <| listToReturn
                , if noMoreRecords then
                    Lamdera.sendToFrontend clientId <| NoMoreTranslationsToFetch publicOrPersonal

                  else
                    Cmd.none
                ]
            )

        RequestTranslation id ->
            case Array.get id model.translationRecords of
                Nothing ->
                    ( model
                    , Lamdera.sendToFrontend clientId <| RequestTranslationRecordsResult <| Err <| InvalidTranslationRecordId
                    )

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
                        Lamdera.sendToFrontend clientId <| RequestTranslationRecordsResult <| Err <| IncorrectPermissionForTranslationRecord
                    )

        SetPostAuthRedirect route ->
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

        RequestEmailLoginCode emailAddress ->
            let
                ( newModel, code ) =
                    EmailCode.getUniqueId model.nowish model
            in
            ( { newModel
                | pendingEmailAuths =
                    model.pendingEmailAuths
                        |> Dict.insert code
                            { email = emailAddress |> EmailAddress.toString
                            , expires = (Time.posixToMillis model.nowish + Config.emailCodeExpirationMillis) |> Time.millisToPosix
                            }
              }
            , Postmark.sendEmail
                (LoginCodeEmailSentResponse ( emailAddress, code ))
                { from = { name = "Login", email = Config.loginCodeFromEmail }
                , to = [ { name = "", email = emailAddress } ]
                , subject = "Eestisse Login Code"
                , body = EmailCode.buildEmailBody code
                , messageStream = "outbound"
                }
            )

        SubmitCodeForEmail emailAddress code ->
            let
                emailAddressString =
                    emailAddress |> EmailAddress.toString
            in
            case Dict.get code model.pendingEmailAuths of
                Nothing ->
                    ( model
                    , Lamdera.sendToFrontend clientId <| LoginCodeError IncorrectCode
                    )

                Just pendingAuth ->
                    if pendingAuth.email /= emailAddressString then
                        ( model
                        , Lamdera.sendToFrontend clientId <| LoginCodeError IncorrectCode
                        )

                    else if Time.Extra.compare model.nowish pendingAuth.expires == GT then
                        ( { model
                            | pendingEmailAuths =
                                model.pendingEmailAuths
                                    |> Dict.remove code
                          }
                        , Lamdera.sendToFrontend clientId <| LoginCodeError CodeExpired
                        )

                    else
                        let
                            modelWithoutPendingAuth =
                                { model
                                    | pendingEmailAuths =
                                        model.pendingEmailAuths
                                            |> Dict.remove code
                                }
                        in
                        Auth.handleAuthSuccess modelWithoutPendingAuth sessionId clientId emailAddressString

        SubmitConsentsForm consentsForm ->
            case maybeUserId of
                Nothing ->
                    model |> notifyAdminOfError "Unexpected: couldn't find user id when getting a SubmitConsentsForm"

                Just userId ->
                    case Dict.get userId model.users of
                        Nothing ->
                            model |> notifyAdminOfError "Unexpected: couldn't find userInfo from a userId when getting a SubmitConsentsForm"

                        Just userInfo ->
                            let
                                newUserInfo =
                                    { userInfo
                                        | consents = Just <| consentsFormToConsents consentsForm
                                    }
                            in
                            ( { model
                                | users =
                                    model.users
                                        |> Dict.insert userId newUserInfo
                              }
                            , Lamdera.sendToFrontend clientId <| UpdateUserInfo <| toFrontendUserInfo ( userId, newUserInfo, Auth.userMembershipStatus model.nowish userInfo )
                            )

        PublicTranslateCheck flag ->
            case maybeUserId of
                Nothing ->
                    ( model, Cmd.none )

                Just userId ->
                    ( { model
                        | users =
                            model.users
                                |> Dict.update
                                    userId
                                    (Maybe.map
                                        (\userInfo ->
                                            { userInfo | publicChecked = flag }
                                        )
                                    )
                      }
                    , Cmd.none
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
                    model
                        |> (notifyAdminOfError <| "no clientReferenceId in StripeSessionCompleted. Session ID: " ++ stripeSessionData.id)
                        |> (\( m, cmd ) ->
                                ( okResponse, m, cmd )
                           )

                ( _, Nothing, _ ) ->
                    model
                        |> (notifyAdminOfError <| "Unexpected: no subscriptionId in StripeSessionCompleted. Session ID: " ++ stripeSessionData.id)
                        |> (\( m, cmd ) ->
                                ( okResponse, m, cmd )
                           )

                ( _, _, Nothing ) ->
                    model
                        |> (notifyAdminOfError <| "Unexpected: no customerId in StripeSessionCompleted. Session ID: " ++ stripeSessionData.id)
                        |> (\( m, cmd ) ->
                                ( okResponse, m, cmd )
                           )

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
                    model
                        |> (notifyAdminOfError <| "no customerId in InvoicePaid. Session ID: " ++ invoiceData.id)
                        |> (\( m, c ) -> ( okResponse, m, c ))

                ( _, Nothing ) ->
                    model
                        |> (notifyAdminOfError <| "no subscrptionId in InvoicePaid. Session ID: " ++ invoiceData.id)
                        |> (\( m, c ) -> ( okResponse, m, c ))

                ( Just _, Just subscriptionId ) ->
                    ( okResponse
                    , model
                    , Stripe.getSubscriptionData subscriptionId SubscriptionDataReceived
                    )



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


notifyAdminOfError : String -> BackendModel -> ( BackendModel, Cmd BackendMsg )
notifyAdminOfError s model =
    let
        emailNeeded =
            Time.Extra.diff
                (Tuple.first Config.intervalWaitBetweenAdminErrorEmails)
                Time.utc
                model.lastAdminAlertEmailSent
                model.nowish
                >= Tuple.second Config.intervalWaitBetweenAdminErrorEmails
    in
    ( { model
        | adminMessages =
            model.adminMessages
                ++ [ ( model.nowish, s ) ]
        , lastAdminAlertEmailSent =
            if emailNeeded then
                model.nowish

            else
                model.lastAdminAlertEmailSent
      }
    , if emailNeeded then
        sendAdminEmailCmd "EESTISSE ERROR" s

      else
        Cmd.none
    )


sendAdminEmailCmd : String -> String -> Cmd BackendMsg
sendAdminEmailCmd subject bodyString =
    Postmark.sendEmail
        (always NoOpBackendMsg)
        { from = { name = "Eestisse Server", email = Config.serverEmail }
        , to = [ { name = "", email = Config.adminEmail } ]
        , subject = subject
        , body = Postmark.BodyText bodyString
        , messageStream = "outbound"
        }


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


consentsFormToConsents : ConsentsFormModel -> UserConsents
consentsFormToConsents form =
    { interview = form.interview
    , features = form.features
    }


clearVeryExpiredLoginCodes : BackendModel -> BackendModel
clearVeryExpiredLoginCodes model =
    { model
        | pendingEmailAuths =
            model.pendingEmailAuths
                |> Dict.filter
                    (\code pendingAuth ->
                        (Time.Extra.compare model.nowish pendingAuth.expires == GT)
                            && (Time.Extra.diff Time.Extra.Hour Time.utc pendingAuth.expires model.nowish > 1)
                    )
    }


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Time.every (toFloat Config.publicUsageConfig.addCreditIntervalMillis) (always AddPublicCredits)
        , Time.every 1000 UpdateBackendNow
        , Time.every (1000 * 60 * 60 * 24) (always Daily)
        , Lamdera.onConnect OnConnect
        ]
