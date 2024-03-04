module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Http
import Json.Decode
import Lamdera exposing (ClientId)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , textInput : String
    , maybeTranslationResult : Maybe (Result GptAssistError Translation)
    }


type GptAssistError
    = ApiProtocolError Http.Error
    | GptDecodeError String
    | GptExpressedError String


type alias Translation =
    { inputAndExplanations : List ( String, String )
    , translation : String
    , selectedExplanation : Maybe ( String, String )
    }


type alias BackendModel =
    { thing : Int }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | TextInputChanged String
    | SubmitText String
    | ShowExplanation String String


type ToBackend
    = NoOpToBackend
    | SubmitTextForTranslation String


type BackendMsg
    = NoOpBackendMsg
    | GptResponseReceived ClientId (Result Http.Error String)


type ToFrontend
    = NoOpToFrontend
    | TranslationResult (Result GptAssistError Translation)
