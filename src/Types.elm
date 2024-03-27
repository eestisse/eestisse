module Types exposing (..)

import Background.Types as Background
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import CommonTypes exposing (..)
import Dict exposing (Dict)
import Http
import Lamdera exposing (ClientId)
import Route exposing (Route)
import Set exposing (Set)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , route : Route
    , dProfile : Maybe DisplayProfile
    , translationPageModel : TranslationPageModel
    , signupState : SignupState
    , maybeImportantNumber : Maybe Int
    , animationTime : Time.Posix
    , backgroundModel : Maybe Background.Model
    }


type FrontendMsg
    = NoOpFrontendMsg
    | UrlClicked UrlRequest
    | UrlChanged Url
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
    | TextInputChanged String
    | SubmitText String
    | ShowExplanation BreakdownPart
    | CycleLoadingAnimation
    | EditTranslation String
    | GotoRoute Route
    | StartSignup
    | SubmitSignup String
    | SignupTextChanged String
    | FetchImportantNumber
    | Animate Time.Posix


type ToBackend
    = NoOpToBackend
    | SubmitTextForTranslation String
    | SubmitEmail String
    | RequestImportantNumber


type BackendMsg
    = NoOpBackendMsg
    | GptResponseReceived ClientId String (Result Http.Error String)
    | AddPublicCredits
    | UpdateNow Time.Posix


type ToFrontend
    = NoOpToFrontend
    | TranslationResult String (Result GptAssistError Translation)
    | EmailSubmitAck
    | ImportantNumber Int


type SignupState
    = Inactive
    | Active String
    | Submitting
    | Submitted


type TranslationPageModel
    = InputtingText String
    | RequestSent RequestState


type RequestState
    = Waiting String Int
    | RequestComplete CompletedRequest


type alias CompletedRequest =
    { inputText : String
    , translationResult : Result GptAssistError Translation
    , maybeSelectedBreakdownPart : Maybe BreakdownPart
    }


type GptAssistError
    = OutOfCredits
    | ApiProtocolError Http.Error
    | GptDecodeError String
    | GptExpressedError String


type alias Translation =
    { breakdown : Breakdown
    , translation : String
    , translatedTo : EnglishOrEstonian
    }


type EnglishOrEstonian
    = English
    | Estonian


type alias Breakdown =
    List BreakdownPart


type alias BreakdownPart =
    { estonian : String
    , englishTranslation : String
    , maybeExplanation : Maybe String
    }


type alias BackendModel =
    { nowish : Time.Posix
    , publicCredits : Int
    , emails : Set String
    , requests : List ( Time.Posix, String, Result GptAssistError Translation )
    }


type alias RGB =
    { red : Float
    , green : Float
    , blue : Float
    }
