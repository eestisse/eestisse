module Backend exposing (..)

import Config
import Env
import GPTRequests
import Http
import Json.Decode
import Lamdera exposing (ClientId, SessionId)
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
    ( { publicCredits = 20 }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        GptResponseReceived clientId input fetchResult ->
            ( case fetchResult of
                Ok _ ->
                    model |> deductOneCredit

                Err _ ->
                    model
            , Lamdera.sendToFrontend clientId
                (TranslationResult input <| GPTRequests.processGptResponse fetchResult)
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


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        SubmitTextForTranslation text ->
            if model.publicCredits > 0 then
                ( model, requestTranslationCmd clientId text )

            else
                ( model
                , Lamdera.sendToFrontend clientId <| TranslationResult text (Err OutOfCredits)
                )


requestTranslationCmd : ClientId -> String -> Cmd BackendMsg
requestTranslationCmd clientId inputText =
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
    Time.every Config.publicUsageConfig.addCreditIntervalMillis (always AddPublicCredits)
