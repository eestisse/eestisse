module Backend exposing (..)

import Auth
import Auth.Flow
import Config
import Dict exposing (Dict)
import Env
import GPTRequests
import Http
import Json.Decode
import Json.Encode
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Result.Extra
import Set
import Stripe
import Time
import Types exposing (..)


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
      , publicCredits = 20
      , emails_backup = Set.empty
      , emailsWithConsents = []
      , requests = []
      , pendingAuths = Dict.empty
      , authedSessions = Dict.empty
      , users = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        OnConnect sessionId clientId ->
            case Dict.get sessionId model.authedSessions of
                Just userInfo ->
                    ( model
                    , Lamdera.sendToFrontend clientId <| AuthSuccess userInfo
                    )

                Nothing ->
                    ( model, Cmd.none )

        AuthBackendMsg authMsg ->
            Auth.Flow.backendUpdate (Auth.backendConfig model) authMsg

        GptResponseReceived clientId publicConsentChecked input fetchResult ->
            let
                gptResult =
                    GPTRequests.processGptResponse fetchResult

                modelWithResult =
                    { model
                        | requests = model.requests |> List.append [ ( model.nowish, ( input, publicConsentChecked ), gptResult ) ]
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
                    (TranslationResult input <| gptResult)
                , bcastCmd
                ]
            )

        AddPublicCredits ->
            let
                newPublicCredits =
                    min
                        Config.publicUsageConfig.maxCapacity
                        (model.publicCredits + Config.publicUsageConfig.addCreditAmount)
            in
            ( { model
                | publicCredits =
                    newPublicCredits
              }
            , if newPublicCredits /= model.publicCredits then
                Lamdera.broadcast <| CreditsUpdated newPublicCredits

              else
                Cmd.none
            )

        UpdateNow time ->
            ( { model | nowish = time }
            , Cmd.none
            )

        UpdateUserDelinquencyStates time ->
            --> check for users paidUntil values against `time`
            --> if not overdue, set subscriptionState = Active
            --> if < 2 days overdue, set subscriptionState = Warning
            --> if > 2 days overdue, set subscriptionState = Delinquent
            Debug.todo ""


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        AuthToBackend authToBackend ->
            Auth.Flow.updateFromFrontend (Auth.backendConfig model) clientId sessionId authToBackend model

        SubmitTextForTranslation publicConsentChecked input ->
            if model.publicCredits > 0 then
                model
                    |> deductOneCreditAndBroadcast
                    |> Tuple.mapSecond
                        (\bcastCmd ->
                            Cmd.batch [ bcastCmd, requestGptTranslationCmd clientId publicConsentChecked input ]
                        )

            else
                ( model
                , Lamdera.sendToFrontend clientId <| TranslationResult input (Err OutOfCredits)
                )

        SubmitSignup signupForm ->
            ( { model
                | emailsWithConsents =
                    model.emailsWithConsents
                        |> List.append [ signupFormToEmailAndConsets signupForm ]
              }
            , Lamdera.sendToFrontend clientId EmailSubmitAck
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
                    , translationSuccesses =
                        model.requests
                            |> List.Extra.count
                                (\( _, _, result ) ->
                                    Result.Extra.isOk result
                                )
                    , translationErrors =
                        model.requests
                            |> List.Extra.count
                                (\( _, _, result ) ->
                                    Result.Extra.isErr result
                                )
                    }
            )

        RequestGeneralData ->
            ( model
            , Lamdera.sendToFrontend clientId <|
                GeneralDataMsg <|
                    GeneralData model.publicCredits
            )


handleStripeWebhook : Stripe.Webhook -> BackendModel -> ( Result Http.Error String, BackendModel, Cmd BackendMsg )
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
            case stripeSessionData.clientReferenceId of
                Nothing ->
                    ( okResponse, model, notifyAdminOfError <| "no clientReferenceId in StripeSessionCompleted. Session ID: " ++ stripeSessionData.id )

                Just userEmail ->
                    let
                        stripeInfo =
                            { customerId = stripeSessionData.customerId
                            , subscriptionId = stripeSessionData.subscriptionId
                            , paidUntil = Nothing
                            }

                        newUser : UserInfo
                        newUser =
                            case Dict.get userEmail model.users of
                                Just alreadyExistingUser ->
                                    { alreadyExistingUser
                                        | stripeInfo = stripeInfo
                                    }

                                Nothing ->
                                    { email = userEmail
                                    , stripeInfo = stripeInfo
                                    }

                        -- todo
                        --> search for matching invoice in hangingInvoices, if found, remove hangingInvoice and store full paid user profile
                        --> save in users
                        ( newModel, cmd ) =
                            Debug.todo ""
                    in
                    ( okResponse
                    , newModel
                    , cmd
                    )

        Stripe.InvoicePaid invoiceData ->
            -- todo
            --> extract subscription id, customer id
            --> get subscription
            --> extract current_period_end
            --> create paidInvoice (customer id, subscription id, paid until)
            --> search for matching user in `users`, if found, update user record
            --> .. if NOT found, add to hangingInvoices
            Debug.todo ""


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


requestGptTranslationCmd : ClientId -> Bool -> String -> Cmd BackendMsg
requestGptTranslationCmd clientId publicConsentChecked inputText =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Env.openaiApiKey) ]
        , url = "https://api.openai.com/v1/chat/completions"
        , body = Http.jsonBody <| GPTRequests.encode <| GPTRequests.translateFromEstonian inputText
        , expect = Http.expectJson (GptResponseReceived clientId publicConsentChecked inputText) GPTRequests.apiResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deductOneCreditAndBroadcast : BackendModel -> ( BackendModel, Cmd BackendMsg )
deductOneCreditAndBroadcast model =
    modifyCreditBalanceAndBroadcast (model.publicCredits - 1) model


refundOneCreditAndBroadcast : BackendModel -> ( BackendModel, Cmd BackendMsg )
refundOneCreditAndBroadcast model =
    modifyCreditBalanceAndBroadcast (model.publicCredits + 1) model


modifyCreditBalanceAndBroadcast : Int -> BackendModel -> ( BackendModel, Cmd BackendMsg )
modifyCreditBalanceAndBroadcast newCredits model =
    ( { model
        | publicCredits = newCredits
      }
    , Lamdera.broadcast <| CreditsUpdated newCredits
    )


notifyAdminOfError : String -> Cmd BackendMsg
notifyAdminOfError s =
    Debug.todo ""


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Time.every Config.publicUsageConfig.addCreditIntervalMillis (always AddPublicCredits)
        , Time.every (1000 * 60 * 60 * 24) UpdateUserDelinquencyStates
        , Time.every 1000 UpdateNow
        , Lamdera.onConnect OnConnect
        ]
