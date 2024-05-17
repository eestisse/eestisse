module Evergreen.V43.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Dict
import Evergreen.V43.Auth.Common
import Evergreen.V43.Background.Types
import Evergreen.V43.Responsive
import Evergreen.V43.Route
import Http
import Lamdera
import Set
import Time
import Url


type alias TranslationInputModel =
    { input : String
    }


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
    = InputtingText TranslationInputModel
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
    , route : Evergreen.V43.Route.Route
    , authFlow : Evergreen.V43.Auth.Common.Flow
    , authRedirectBaseUrl : Url.Url
    , dProfile : Maybe Evergreen.V43.Responsive.DisplayProfile
    , translationPageModel : TranslationPageModel
    , publicConsentChecked : Bool
    , signupState : SignupState
    , maybeAdminData : Maybe AdminData
    , animationTime : Time.Posix
    , backgroundModel : Maybe Evergreen.V43.Background.Types.Model
    , publicCredits : Maybe Int
    , showCreditCounterTooltip : Bool
    , creditsCounterAnimationState : Maybe CreditsCounterAnimationState
    }


type alias EmailAndConsents =
    { email : String
    , consentsGiven : List String
    }


type alias UserInfo =
    { email : String
    }


type alias BackendModel =
    { nowish : Time.Posix
    , publicCredits : Int
    , emails_backup : Set.Set String
    , emailsWithConsents : List EmailAndConsents
    , requests : List ( Time.Posix, ( String, Bool ), Result GptAssistError Translation )
    , pendingAuths : Dict.Dict Lamdera.SessionId Evergreen.V43.Auth.Common.PendingAuth
    , sessions : Dict.Dict Lamdera.SessionId UserInfo
    }


type FrontendMsg
    = NoOpFrontendMsg
    | AuthSigninRequested
        { methodId : Evergreen.V43.Auth.Common.MethodId
        , username : Maybe String
        }
    | UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
    | TranslationInputModelChanged TranslationInputModel
    | SubmitText Bool String
    | ShowExplanation BreakdownPart
    | CycleLoadingAnimation
    | EditTranslation String
    | GotoRoute Evergreen.V43.Route.Route
    | GotoTranslate_FocusAndClear
    | StartSignup
    | SubmitSignupClicked SignupFormModel
    | SignupFormChanged SignupFormModel
    | FetchImportantNumber
    | Animate Time.Posix
    | FiddleRandomBackroundPath Time.Posix
    | ShowCreditCounterTooltip Bool
    | SetPublicConsentChecked Bool


type ToBackend
    = NoOpToBackend
    | AuthToBackend Evergreen.V43.Auth.Common.ToBackend
    | SubmitTextForTranslation Bool String
    | SubmitSignup SignupFormModel
    | RequestImportantNumber
    | RequestGeneralData


type BackendMsg
    = NoOpBackendMsg
    | AuthBackendMsg Evergreen.V43.Auth.Common.BackendMsg
    | GptResponseReceived Lamdera.ClientId Bool String (Result Http.Error String)
    | AddPublicCredits
    | UpdateNow Time.Posix


type alias GeneralData =
    { publicCredits : Int
    }


type ToFrontend
    = NoOpToFrontend
    | AuthToFrontend Evergreen.V43.Auth.Common.ToFrontend
    | AuthSuccess Evergreen.V43.Auth.Common.UserInfo
    | TranslationResult String (Result GptAssistError Translation)
    | EmailSubmitAck
    | AdminDataMsg AdminData
    | GeneralDataMsg GeneralData
    | CreditsUpdated Int
