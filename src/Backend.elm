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
import Json.Encode
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
    ( { time_bySecond = Time.millisToPosix 0
      , publicCreditsInfo =
            -- dummy value - replaced below in response to InitialTimeVal
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
      , timeOfLastAdminMessageRead = Time.millisToPosix 0
      , dummyVal = ""
      }
    , Time.now
        |> Task.perform InitialTimeVal
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        B_NoOp ->
            ( model, Cmd.none )

        Daily ->
            ( model |> clearVeryExpiredLoginCodes
            , Cmd.none
            )

        InitialTimeVal t ->
            ( { model
                | time_bySecond = t
                , publicCreditsInfo =
                    { current = 10
                    , nextRefresh =
                        Time.posixToMillis t
                            + Config.publicUsageConfig.addCreditIntervalMillis
                            |> Time.millisToPosix
                    , refreshAmount = Config.publicUsageConfig.addCreditAmount
                    }
              }
            , Cmd.none
            )

        UpdateBackendNow_BySecond time ->
            ( { model | time_bySecond = time }
            , Cmd.none
            )

        OnConnect sessionId clientId ->
            let
                ( newSessions, sessionInfo ) =
                    case Dict.get sessionId model.sessions of
                        Nothing ->
                            ( model.sessions
                                |> Dict.insert
                                    sessionId
                                    blankSession
                            , blankSession
                            )

                        Just session ->
                            ( model.sessions
                            , session
                            )

                maybeUserIdAndInfo =
                    sessionInfo.maybeAuthedUserId
                        |> Maybe.andThen
                            (\id ->
                                model.users
                                    |> Dict.get id
                                    |> Maybe.map (Tuple.pair id)
                            )

                userInfoUpdateCmd =
                    case maybeUserIdAndInfo of
                        Nothing ->
                            Lamdera.sendToFrontend clientId <| TF_UserInfo Nothing

                        Just ( userId, userInfo ) ->
                            Lamdera.sendToFrontend clientId <| TF_AuthSuccess <| toFrontendUserInfo userId userInfo model.time_bySecond

                newModel =
                    { model
                        | sessions = newSessions
                    }

                generalDataCmd =
                    Lamdera.sendToFrontend clientId <| TF_GeneralData <| getGeneralDataFromModel newModel
            in
            ( newModel
            , Cmd.batch
                [ userInfoUpdateCmd
                , generalDataCmd
                ]
            )

        AuthBackendMsg authMsg ->
            Auth.Flow.backendUpdate (Auth.backendConfig model) authMsg

        GptResponseReceived ( sessionId, clientId ) publicConsentChecked input fetchResult ->
            let
                translationRecordResult =
                    GPTRequests.processGptResponse fetchResult
                        |> Result.map
                            (TranslationRecord (Array.length model.translationRecords) (Maybe.map Tuple.first <| sessionIdToMaybeUserIdAndInfo sessionId model) publicConsentChecked model.time_bySecond input)

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
                    (TF_TranslationResult input <| translationRecordResult)
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
                        Time.posixToMillis model.time_bySecond
                            + Config.publicUsageConfig.addCreditIntervalMillis
                            |> Time.millisToPosix
                    , refreshAmount = Config.publicUsageConfig.addCreditAmount
                    }
            in
            ( { model
                | publicCreditsInfo =
                    newPublicCreditsInfo
              }
            , Lamdera.broadcast <| TF_CreditsInfo newPublicCreditsInfo
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

                                numPayingUsers =
                                    countPaidUsers newUsers
                            in
                            ( { model
                                | users = newUsers
                              }
                            , if Config.numEarlybirdOffersTotal - numPayingUsers >= Config.earlybirdOffersLeftAlertThreshold then
                                sendAdminEmailCmd "shit's getting good!" ("Look out! Only " ++ String.fromInt (Config.numEarlybirdOffersTotal - numPayingUsers) ++ " earlybird specials left!")

                              else
                                Cmd.none
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
        ( maybeUserId, maybeUserInfo ) =
            case sessionIdToMaybeUserIdAndInfo sessionId model of
                Just ( userId, userInfo ) ->
                    ( Just userId, Just userInfo )

                Nothing ->
                    ( Nothing, Nothing )
    in
    case msg of
        TB_NoOp ->
            ( model, Cmd.none )

        TB_AuthMsg authToBackend ->
            Auth.Flow.updateFromFrontend (Auth.backendConfig model) clientId sessionId authToBackend model

        TB_TextForTranslation publicConsentChecked input ->
            if maybeBackendUserInfoMembershipActive maybeUserInfo model.time_bySecond then
                ( model
                , requestGptTranslationCmd ( sessionId, clientId ) publicConsentChecked input
                )

            else if model.publicCreditsInfo.current > 0 then
                model
                    |> deductOneCreditAndBroadcast
                    |> Tuple.mapSecond
                        (\bcastCmd ->
                            Cmd.batch [ bcastCmd, requestGptTranslationCmd ( sessionId, clientId ) publicConsentChecked input ]
                        )

            else
                ( model
                , Lamdera.sendToFrontend clientId <| TF_TranslationResult input (Err OutOfCredits)
                )

        R_AdminData ->
            if maybeUserIdIsAdmin maybeUserId model then
                ( model
                , Lamdera.sendToFrontend clientId <|
                    TF_AdminData <|
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
                        , adminMessages =
                            model.adminMessages
                                |> List.filter
                                    (\( t, _ ) ->
                                        Time.Extra.compare t model.timeOfLastAdminMessageRead == GT
                                    )
                        , numPaidUsers =
                            countPaidUsers model.users
                        }
                )

            else
                ( model, Cmd.none )

        R_GeneralData ->
            ( model
            , Lamdera.sendToFrontend clientId <| TF_GeneralData <| getGeneralDataFromModel model
            )

        TB_Logout ->
            Auth.logout sessionId clientId model
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Lamdera.sendToFrontend clientId <| TF_UserInfo <| Nothing
                            ]
                    )

        R_TranslationRecords publicOrPersonal ( maybeLowestCachedId, count ) ->
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
                [ Lamdera.sendToFrontend clientId <| TF_TranslationRecordsRequestResult <| Ok <| listToReturn
                , if noMoreRecords then
                    Lamdera.sendToFrontend clientId <| TF_NoMoreTranslationsToFetch publicOrPersonal

                  else
                    Cmd.none
                ]
            )

        R_SingleTranslationRecord id ->
            case Array.get id model.translationRecords of
                Nothing ->
                    ( model
                    , Lamdera.sendToFrontend clientId <| TF_TranslationRecordsRequestResult <| Err <| InvalidTranslationRecordId
                    )

                Just translationRecord ->
                    let
                        hasPermission =
                            translationRecord.public || (maybeUserId == translationRecord.fromUserId)
                    in
                    ( model
                    , if hasPermission then
                        Lamdera.sendToFrontend clientId <|
                            TF_TranslationRecordsRequestResult <|
                                Ok <|
                                    List.singleton <|
                                        translationRecord

                      else
                        Lamdera.sendToFrontend clientId <| TF_TranslationRecordsRequestResult <| Err <| IncorrectPermissionForTranslationRecord
                    )

        TB_SetPostAuthRedirect route ->
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

        R_AndClearRedirectReturnPage ->
            ( model |> clearRedirectReturnPageForSession sessionId
            , Lamdera.sendToFrontend clientId <|
                TF_RedirectReturnPage
                    (model.sessions
                        |> Dict.get sessionId
                        |> Maybe.andThen .redirectReturnPage
                    )
            )

        R_EmailLoginCode emailAddress ->
            let
                ( newModel, code ) =
                    EmailCode.getUniqueId model.time_bySecond model
            in
            ( { newModel
                | pendingEmailAuths =
                    model.pendingEmailAuths
                        |> Dict.insert code
                            { email = emailAddress |> EmailAddress.toString
                            , expires = (Time.posixToMillis model.time_bySecond + Config.emailCodeExpirationMillis) |> Time.millisToPosix
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

        TB_EmailSigninCode emailAddress code ->
            let
                emailAddressString =
                    emailAddress |> EmailAddress.toString
            in
            case Dict.get code model.pendingEmailAuths of
                Nothing ->
                    ( model
                    , Lamdera.sendToFrontend clientId <| TF_LoginCodeError IncorrectCode
                    )

                Just pendingAuth ->
                    if pendingAuth.email /= emailAddressString then
                        ( model
                        , Lamdera.sendToFrontend clientId <| TF_LoginCodeError IncorrectCode
                        )

                    else if Time.Extra.compare model.time_bySecond pendingAuth.expires == GT then
                        ( { model
                            | pendingEmailAuths =
                                model.pendingEmailAuths
                                    |> Dict.remove code
                          }
                        , Lamdera.sendToFrontend clientId <| TF_LoginCodeError CodeExpired
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

        TB_Consents consentsForm ->
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
                            , Lamdera.sendToFrontend clientId <| TF_UserInfo <| Just <| toFrontendUserInfo userId newUserInfo model.time_bySecond
                            )

        TB_SetPublicTranslateChecked flag ->
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

        TB_UserFeedback isUser maybeEmail feedbackText ->
            let
                subject =
                    "user feedback from " ++ (maybeEmail |> Maybe.withDefault "anon")

                body =
                    "This just in "
                        ++ (if isUser then
                                "from a signed up user"

                            else
                                "from a non-signed-up user"
                           )
                        ++ ":\n\n"
                        ++ feedbackText
            in
            ( model
            , Cmd.batch
                [ sendAdminEmailCmd subject body
                , Lamdera.sendToFrontend sessionId TF_AckUserFeedback
                ]
            )

        TB_SetAdminMessagesLastRead time ->
            if maybeUserIdIsAdmin maybeUserId model then
                ( { model | timeOfLastAdminMessageRead = time }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

        TB_TestAdminError s ->
            notifyAdminOfError ("Test error: " ++ s) model


handleStripeWebhook : Stripe.StripeEvent -> BackendModel -> ( Result Http.Error Json.Encode.Value, BackendModel, Cmd BackendMsg )
handleStripeWebhook webhook model =
    let
        okResponse =
            case Env.mode of
                Env.Development ->
                    Ok <| Json.Encode.string "prod"

                Env.Production ->
                    Ok <| Json.Encode.string "dev"
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
    , Lamdera.broadcast <| TF_CreditsInfo newPublicCreditsInfo
    )


notifyAdminOfError : String -> BackendModel -> ( BackendModel, Cmd BackendMsg )
notifyAdminOfError s model =
    let
        emailNeeded =
            Time.Extra.diff
                (Tuple.first Config.intervalWaitBetweenAdminErrorEmails)
                Time.utc
                model.lastAdminAlertEmailSent
                model.time_bySecond
                >= Tuple.second Config.intervalWaitBetweenAdminErrorEmails

        subject =
            "EESTISSE ERROR"
                ++ (let
                        numAdditionalMessagesCached =
                            model.adminMessages
                                |> List.filter
                                    (\( t, _ ) ->
                                        Time.Extra.compare t model.lastAdminAlertEmailSent == GT
                                    )
                                |> List.length
                    in
                    if numAdditionalMessagesCached == 0 then
                        ""

                    else
                        " (+ " ++ String.fromInt numAdditionalMessagesCached ++ " more errors cached)"
                   )
    in
    ( { model
        | adminMessages =
            model.adminMessages
                ++ [ ( model.time_bySecond, s ) ]
        , lastAdminAlertEmailSent =
            if emailNeeded then
                model.time_bySecond

            else
                model.lastAdminAlertEmailSent
      }
    , if emailNeeded then
        sendAdminEmailCmd subject s

      else
        Cmd.none
    )


sendAdminEmailCmd : String -> String -> Cmd BackendMsg
sendAdminEmailCmd subject bodyString =
    Postmark.sendEmail
        (always B_NoOp)
        { from = { name = "Eestisse Server", email = Config.serverEmail }
        , to = [ { name = "", email = Config.mainAdminEmail } ]
        , subject = subject
        , body = Postmark.BodyText bodyString
        , messageStream = "outbound"
        }


sessionIdToMaybeUserIdAndInfo : SessionId -> BackendModel -> Maybe ( Int, UserInfo )
sessionIdToMaybeUserIdAndInfo sessionId model =
    model.sessions
        |> Dict.get sessionId
        |> Maybe.andThen .maybeAuthedUserId
        |> Maybe.andThen
            (\id ->
                Dict.get id model.users |> Maybe.map (Tuple.pair id)
            )


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
                        (Time.Extra.compare model.time_bySecond pendingAuth.expires == GT)
                            && (Time.Extra.diff Time.Extra.Hour Time.utc pendingAuth.expires model.time_bySecond > 1)
                    )
    }


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Time.every (toFloat Config.publicUsageConfig.addCreditIntervalMillis) (always AddPublicCredits)
        , Time.every 1000 UpdateBackendNow_BySecond
        , Time.every (1000 * 60 * 60 * 24) (always Daily)
        , Lamdera.onConnect OnConnect
        ]


maybeUserIdIsAdmin : Maybe Int -> BackendModel -> Bool
maybeUserIdIsAdmin maybeUserId model =
    case maybeUserId of
        Just userId ->
            case Dict.get userId model.users of
                Just userInfo ->
                    Config.adminEmails
                        |> List.map EmailAddress.toString
                        |> List.member userInfo.email

                Nothing ->
                    False

        Nothing ->
            False


countPaidUsers : Dict Int UserInfo -> Int
countPaidUsers users =
    users
        |> Dict.filter
            (\_ userInfo ->
                (userInfo.stripeInfo |> Maybe.andThen .paidUntil) /= Nothing
            )
        |> Dict.toList
        |> List.length
