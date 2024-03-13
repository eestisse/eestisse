module Evergreen.V3.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V3.Route
import Http
import Lamdera
import Url


type GptAssistError
    = OutOfCredits
    | ApiProtocolError Http.Error
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
    = Waiting String Int
    | RequestComplete CompletedRequest


type TranslationPageModel
    = InputtingText String
    | RequestSent RequestState


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , route : Evergreen.V3.Route.Route
    , showExplainerSubtitle : Bool
    , translationPageModel : TranslationPageModel
    }


type alias BackendModel =
    { publicCredits : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextInputChanged String
    | SubmitText String
    | ShowExplanation BreakdownPart
    | CycleLoadingAnimation
    | EditTranslation String
    | HideExplainer
    | GotoRoute Evergreen.V3.Route.Route


type ToBackend
    = NoOpToBackend
    | SubmitTextForTranslation String


type BackendMsg
    = NoOpBackendMsg
    | GptResponseReceived Lamdera.ClientId String (Result Http.Error String)
    | AddPublicCredits


type ToFrontend
    = NoOpToFrontend
    | TranslationResult String (Result GptAssistError Translation)
