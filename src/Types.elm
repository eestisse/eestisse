module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Http
import Json.Decode
import Lamdera exposing (ClientId)
import Route exposing (Route)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , route : Route
    , translationPageModel : TranslationPageModel
    }


type alias TranslationPageModel =
    { textInput : String
    , requestState : RequestState
    }


type RequestState
    = NotSubmitted
    | Loading String
    | RequestComplete CompletedRequest


type alias CompletedRequest =
    { inputText : String
    , translationResult : Result GptAssistError Translation
    , maybeSelectedBreakdownPart : Maybe BreakdownPart
    }


type GptAssistError
    = ApiProtocolError Http.Error
    | GptDecodeError String
    | GptExpressedError String


type alias Translation =
    { breakdown : Breakdown
    , translation : String
    }


type alias Breakdown =
    List BreakdownPart


type alias BreakdownPart =
    { estonian : String
    , englishTranslation : String
    , maybeExplanation : Maybe String
    }


type alias BackendModel =
    { thing : Int }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | TextInputChanged String
    | SubmitText String
    | ShowExplanation BreakdownPart


type ToBackend
    = NoOpToBackend
    | SubmitTextForTranslation String


type BackendMsg
    = NoOpBackendMsg
    | GptResponseReceived ClientId String (Result Http.Error String)


type ToFrontend
    = NoOpToFrontend
    | TranslationResult String (Result GptAssistError Translation)
