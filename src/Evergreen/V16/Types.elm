module Evergreen.V16.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V16.Route
import Http
import Lamdera
import Set
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


type SignupState
    = Inactive
    | Active String
    | Submitting
    | Submitted


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , route : Evergreen.V16.Route.Route
    , translationPageModel : TranslationPageModel
    , signupState : SignupState
    , maybeImportantNumber : Maybe Int
    }


type alias BackendModel =
    { publicCredits : Int
    , emails : Set.Set String
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
    | GotoRoute Evergreen.V16.Route.Route
    | StartSignup
    | SubmitSignup String
    | SignupTextChanged String
    | FetchImportantNumber


type ToBackend
    = NoOpToBackend
    | SubmitTextForTranslation String
    | SubmitEmail String
    | RequestImportantNumber


type BackendMsg
    = NoOpBackendMsg
    | GptResponseReceived Lamdera.ClientId String (Result Http.Error String)
    | AddPublicCredits


type ToFrontend
    = NoOpToFrontend
    | TranslationResult String (Result GptAssistError Translation)
    | EmailSubmitAck
    | ImportantNumber Int
