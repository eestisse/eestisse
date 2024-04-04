module Evergreen.V33.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Evergreen.V33.Background.Types
import Evergreen.V33.Responsive
import Evergreen.V33.Route
import Http
import Lamdera
import Set
import Time
import Url


type ProtocolError
    = RateLimited
    | HttpError Http.Error


type GptAssistError
    = OutOfCredits
    | ApiProtocolError ProtocolError
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


type alias SignupFormModel =
    { emailInput : String
    , newFeaturesConsentChecked : Bool
    , userInterviewsConsentChecked : Bool
    }


type SignupState
    = Inactive
    | Active SignupFormModel
    | Submitting
    | Submitted


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , route : Evergreen.V33.Route.Route
    , dProfile : Maybe Evergreen.V33.Responsive.DisplayProfile
    , translationPageModel : TranslationPageModel
    , signupState : SignupState
    , maybeImportantNumbers : Maybe (List ( String, Int ))
    , animationTime : Time.Posix
    , backgroundModel : Maybe Evergreen.V33.Background.Types.Model
    }


type alias EmailAndConsents =
    { email : String
    , consentsGiven : List String
    }


type alias BackendModel =
    { nowish : Time.Posix
    , publicCredits : Int
    , emails_backup : Set.Set String
    , emailsWithConsents : List EmailAndConsents
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
    | GotoRoute Evergreen.V33.Route.Route
    | GotoTranslate_FocusAndClear
    | StartSignup
    | SubmitSignupClicked SignupFormModel
    | SignupFormChanged SignupFormModel
    | FetchImportantNumber
    | Animate Time.Posix
    | FiddleRandomBackroundPath Time.Posix


type ToBackend
    = NoOpToBackend
    | SubmitTextForTranslation String
    | SubmitSignup SignupFormModel
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
    | ImportantNumbers (List ( String, Int ))
