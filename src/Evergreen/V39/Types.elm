module Evergreen.V39.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Evergreen.V39.Background.Types
import Evergreen.V39.Responsive
import Evergreen.V39.Route
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


type alias AdminData =
    { emailsAndConsents : List ( String, Int )
    , translationSuccesses : Int
    , translationErrors : Int
    }


type alias CreditsCounterAnimationState =
    { goingUp : Bool
    , startTime : Time.Posix
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , route : Evergreen.V39.Route.Route
    , dProfile : Maybe Evergreen.V39.Responsive.DisplayProfile
    , translationPageModel : TranslationPageModel
    , signupState : SignupState
    , maybeAdminData : Maybe AdminData
    , animationTime : Time.Posix
    , backgroundModel : Maybe Evergreen.V39.Background.Types.Model
    , publicCredits : Maybe Int
    , showCreditCounterTooltip : Bool
    , creditsCounterAnimationState : Maybe CreditsCounterAnimationState
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
    | GotoRoute Evergreen.V39.Route.Route
    | GotoTranslate_FocusAndClear
    | StartSignup
    | SubmitSignupClicked SignupFormModel
    | SignupFormChanged SignupFormModel
    | FetchImportantNumber
    | Animate Time.Posix
    | FiddleRandomBackroundPath Time.Posix
    | ShowCreditCounterTooltip Bool


type ToBackend
    = NoOpToBackend
    | SubmitTextForTranslation String
    | SubmitSignup SignupFormModel
    | RequestImportantNumber
    | RequestGeneralData


type BackendMsg
    = NoOpBackendMsg
    | GptResponseReceived Lamdera.ClientId String (Result Http.Error String)
    | AddPublicCredits
    | UpdateNow Time.Posix


type alias GeneralData =
    { publicCredits : Int
    }


type ToFrontend
    = NoOpToFrontend
    | TranslationResult String (Result GptAssistError Translation)
    | EmailSubmitAck
    | AdminDataMsg AdminData
    | GeneralDataMsg GeneralData
    | CreditsUpdated Int
