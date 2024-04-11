module Types exposing (..)

import Auth.Common
import Background.Types as Background
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Http
import Lamdera exposing (ClientId, SessionId)
import Responsive exposing (..)
import Route exposing (Route)
import Set exposing (Set)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , route : Route
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    , userInfo : Maybe UserInfo
    , dProfile : Maybe DisplayProfile
    , translationPageModel : TranslationPageModel
    , signupState : SignupState
    , maybeAdminData : Maybe AdminData
    , animationTime : Time.Posix
    , backgroundModel : Maybe Background.Model
    , publicCredits : Maybe Int
    , showCreditCounterTooltip : Bool
    , creditsCounterAnimationState : Maybe CreditsCounterAnimationState
    }


type alias BackendModel =
    { nowish : Time.Posix
    , publicCredits : Int
    , emails_backup : Set String
    , emailsWithConsents : List EmailAndConsents
    , requests : List ( Time.Posix, String, Result GptAssistError Translation )
    , pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth
    , authedSessions : Dict Lamdera.SessionId UserInfo
    }


type FrontendMsg
    = NoOpFrontendMsg
    | AuthSigninRequested { methodId : Auth.Common.MethodId, username : Maybe String }
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
    | GotoTranslate_FocusAndClear
    | StartSignup
    | SubmitSignupClicked SignupFormModel
    | SignupFormChanged SignupFormModel
    | FetchImportantNumber
    | Animate Time.Posix
    | FiddleRandomBackroundPath Time.Posix
    | ShowCreditCounterTooltip Bool


type BackendMsg
    = NoOpBackendMsg
    | AuthBackendMsg Auth.Common.BackendMsg
    | GptResponseReceived ClientId String (Result Http.Error String)
    | AddPublicCredits
    | UpdateNow Time.Posix
    | OnConnect SessionId ClientId


type ToBackend
    = NoOpToBackend
    | AuthToBackend Auth.Common.ToBackend
    | SubmitTextForTranslation String
    | SubmitSignup SignupFormModel
    | RequestImportantNumber
    | RequestGeneralData


type ToFrontend
    = NoOpToFrontend
    | AuthToFrontend Auth.Common.ToFrontend
    | AuthSuccess UserInfo
    | TranslationResult String (Result GptAssistError Translation)
    | EmailSubmitAck
    | AdminDataMsg AdminData
    | GeneralDataMsg GeneralData
    | CreditsUpdated Int


type alias UserInfo =
    { email : String }


type alias AdminData =
    { emailsAndConsents : List ( String, Int )
    , translationSuccesses : Int
    , translationErrors : Int
    }


type alias CreditsCounterAnimationState =
    { goingUp : Bool
    , startTime : Time.Posix
    }


type alias GeneralData =
    { publicCredits : Int }


type alias EmailAndConsents =
    { email : String
    , consentsGiven : List String
    }


type SignupState
    = Inactive
    | Active SignupFormModel
    | Submitting
    | Submitted


type alias SignupFormModel =
    { emailInput : String
    , newFeaturesConsentChecked : Bool
    , userInterviewsConsentChecked : Bool
    }


blankSignupForm : SignupFormModel
blankSignupForm =
    SignupFormModel
        ""
        False
        False


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
    | ApiProtocolError ProtocolError
    | GptDecodeError String
    | GptExpressedError String


type ProtocolError
    = RateLimited
    | HttpError Http.Error


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


type alias RGB =
    { red : Float
    , green : Float
    , blue : Float
    }
