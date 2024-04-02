module Evergreen.V27.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Evergreen.V27.Background.Types
import Evergreen.V27.Responsive
import Evergreen.V27.Route
import Http
import Lamdera
import Set
import Time
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


type EnglishOrEstonian
    = English
    | Estonian


type alias Translation =
    { breakdown : Breakdown
    , translation : String
    , translatedTo : EnglishOrEstonian
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
    , route : Evergreen.V27.Route.Route
    , dProfile : Maybe Evergreen.V27.Responsive.DisplayProfile
    , translationPageModel : TranslationPageModel
    , signupState : SignupState
    , maybeImportantNumber : Maybe Int
    , animationTime : Time.Posix
    , backgroundModel : Maybe Evergreen.V27.Background.Types.Model
    }


type alias BackendModel =
    { nowish : Time.Posix
    , publicCredits : Int
    , emails : Set.Set String
    , requests : List ( Time.Posix, String, Result GptAssistError Translation )
    }


type FrontendMsg
    = NoOpFrontendMsg
    | UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
    | TextInputChanged String
    | SubmitText String
    | ShowExplanation BreakdownPart
    | CycleLoadingAnimation
    | EditTranslation String
    | GotoRoute Evergreen.V27.Route.Route
    | GotoTranslateAndFocus
    | StartSignup
    | SubmitSignup String
    | SignupTextChanged String
    | FetchImportantNumber
    | Animate Time.Posix
    | FiddleRandomBackroundPath Time.Posix


type ToBackend
    = NoOpToBackend
    | SubmitTextForTranslation String
    | SubmitEmail String
    | RequestImportantNumber


type BackendMsg
    = NoOpBackendMsg
    | GptResponseReceived Lamdera.ClientId String (Result Http.Error String)
    | AddPublicCredits
    | UpdateNow Time.Posix


type ToFrontend
    = NoOpToFrontend
    | TranslationResult String (Result GptAssistError Translation)
    | EmailSubmitAck
    | ImportantNumber Int
