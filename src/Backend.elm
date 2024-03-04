module Backend exposing (..)

import Env
import GPTRequests
import Http
import Json.Decode
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { thing = 0 }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        GptResponseReceived clientId fetchResult ->
            ( model
            , Lamdera.sendToFrontend clientId
                (TranslationResult <| GPTRequests.processGptResponse fetchResult)
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        SubmitTextForTranslation text ->
            ( model, requestTranslationCmd clientId text )


requestTranslationCmd : ClientId -> String -> Cmd BackendMsg
requestTranslationCmd clientId inputText =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Env.openaiApiKey) ]
        , url = "https://api.openai.com/v1/chat/completions"
        , body = Http.jsonBody <| GPTRequests.encode <| GPTRequests.translateFromEstonian inputText
        , expect = Http.expectJson (GptResponseReceived clientId) GPTRequests.apiResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
