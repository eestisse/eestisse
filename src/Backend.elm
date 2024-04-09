module Backend exposing (..)

-- import ClaudeRequests

import Config
import Env
import GPTRequests
import Http
import Json.Decode
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Set
import Time
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { nowish = Time.millisToPosix 0
      , publicCredits = 20
      , emails_backup = Set.empty
      , emailsWithConsents = []
      , requests = []
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        GptResponseReceived clientId input fetchResult ->
            let
                gptResult =
                    GPTRequests.processGptResponse fetchResult

                modelWithResult =
                    { model
                        | requests = model.requests |> List.append [ ( model.nowish, input, gptResult ) ]
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


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        SubmitTextForTranslation text ->
            if model.publicCredits > 0 then
                model
                    |> deductOneCreditAndBroadcast
                    |> Tuple.mapSecond
                        (\bcastCmd ->
                            Cmd.batch [ bcastCmd, requestGptTranslationCmd clientId text ]
                        )

            else
                ( model
                , Lamdera.sendToFrontend clientId <| TranslationResult text (Err OutOfCredits)
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
                ImportantNumbers <|
                    (model.emailsWithConsents
                        |> List.map
                            (\emailWithConsents ->
                                emailWithConsents.consentsGiven
                                    |> List.map (\consent -> ( emailWithConsents.email, consent ))
                            )
                        |> List.concat
                        |> List.Extra.unique
                        |> List.map Tuple.second
                        |> List.Extra.frequencies
                    )
            )

        RequestGeneralData ->
            ( model
            , Lamdera.sendToFrontend clientId <|
                GeneralDataMsg <|
                    GeneralData model.publicCredits
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


requestGptTranslationCmd : ClientId -> String -> Cmd BackendMsg
requestGptTranslationCmd clientId inputText =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Env.openaiApiKey) ]
        , url = "https://api.openai.com/v1/chat/completions"
        , body = Http.jsonBody <| GPTRequests.encode <| GPTRequests.translateFromEstonian inputText
        , expect = Http.expectJson (GptResponseReceived clientId inputText) GPTRequests.apiResponseDecoder
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


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Time.every Config.publicUsageConfig.addCreditIntervalMillis (always AddPublicCredits)
        , Time.every 1000 UpdateNow
        ]
