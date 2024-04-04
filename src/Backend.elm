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
            in
            ( case fetchResult of
                Ok _ ->
                    modelWithResult |> deductOneCredit

                Err _ ->
                    modelWithResult
            , Lamdera.sendToFrontend clientId
                (TranslationResult input <| gptResult)
            )

        AddPublicCredits ->
            ( { model
                | publicCredits =
                    min
                        Config.publicUsageConfig.maxCapacity
                        (model.publicCredits + Config.publicUsageConfig.addCreditAmount)
              }
            , Cmd.none
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
                ( model, requestGptTranslationCmd clientId text )

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
                        |> List.map .consentsGiven
                        |> List.concat
                        |> List.Extra.frequencies
                    )
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


deductOneCredit : BackendModel -> BackendModel
deductOneCredit model =
    { model
        | publicCredits = model.publicCredits - 1
    }


subscriptions : BackendModel -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Time.every Config.publicUsageConfig.addCreditIntervalMillis (always AddPublicCredits)
        , Time.every 1000 UpdateNow
        ]
