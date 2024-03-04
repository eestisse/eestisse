module Evergreen.V3.Types exposing (..)

import Browser
import Browser.Navigation
import Http
import Lamdera
import Url


type GptAssistError
    = ApiProtocolError Http.Error
    | GptDecodeError String
    | GptExpressedError String


type alias BreakdownPart =
    { estonian : String
    , englishTranslation : String
    , maybeExplanation : Maybe String
    }


type alias Breakdown =
    List BreakdownPart


type alias Translation =
    { breakdown : Breakdown
    , translation : String
    }


type alias CompletedRequest =
    { inputText : String
    , translationResult : Result GptAssistError Translation
    , maybeSelectedBreakdownPart : Maybe BreakdownPart
    }


type RequestState
    = NotSubmitted
    | Loading String
    | RequestComplete CompletedRequest


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , textInput : String
    , requestState : RequestState
    }


type alias BackendModel =
    { thing : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextInputChanged String
    | SubmitText String
    | ShowExplanation BreakdownPart


type ToBackend
    = NoOpToBackend
    | SubmitTextForTranslation String


type BackendMsg
    = NoOpBackendMsg
    | GptResponseReceived Lamdera.ClientId String (Result Http.Error String)


type ToFrontend
    = NoOpToFrontend
    | TranslationResult String (Result GptAssistError Translation)
