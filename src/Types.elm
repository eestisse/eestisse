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
import Stripe.Types as Stripe
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , route : Route
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    , authedUserEmail : Maybe String
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
    , requests : List ( Time.Posix, ( String, Bool ), Result GptAssistError Translation )
    , pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth
    , authedSessions : Dict Lamdera.SessionId String
    , users : Dict String UserInfo
    , hangingInvoices : List PaidInvoice
    }


type FrontendMsg
    = NoOpFrontendMsg
    | AuthSigninRequested { methodId : Auth.Common.MethodId, username : Maybe String }
    | UrlClicked UrlRequest
    | UrlChanged Url
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
    | TranslationInputModelChanged TranslationInputModel
    | SubmitText Bool String
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
    | TriggerStripePayment String


type BackendMsg
    = NoOpBackendMsg
    | AuthBackendMsg Auth.Common.BackendMsg
    | GptResponseReceived ClientId Bool String (Result Http.Error String)
    | AddPublicCredits
    | UpdateNow Time.Posix
    | OnConnect SessionId ClientId
    | SubscriptionDataReceived (Result Http.Error Stripe.SubscriptionData)
    | UpdateUserDelinquencyStates Time.Posix


type ToBackend
    = NoOpToBackend
    | AuthToBackend Auth.Common.ToBackend
    | SubmitTextForTranslation Bool String
    | SubmitSignup SignupFormModel
    | RequestImportantNumber
    | RequestGeneralData


type ToFrontend
    = NoOpToFrontend
    | AuthToFrontend Auth.Common.ToFrontend
    | AuthSuccess String
    | TranslationResult String (Result GptAssistError Translation)
    | EmailSubmitAck
    | AdminDataMsg AdminData
    | GeneralDataMsg GeneralData
    | CreditsUpdated Int


type alias PaidInvoice =
    { customerId : String
    , paidUntil : Time.Posix
    }


type alias UserInfo =
    { email : String
    , stripeInfo : StripeInfo
    }


type alias StripeInfo =
    { customerId : String
    , subscriptionId : String
    , paidUntil : Maybe Time.Posix
    }


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
    = InputtingText TranslationInputModel
    | RequestSent RequestState


type alias TranslationInputModel =
    { input : String
    , publicConsentChecked : Bool
    }


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


updateUserPaidUntil : Time.Posix -> UserInfo -> UserInfo
updateUserPaidUntil paidUntil userInfo =
    { userInfo
        | stripeInfo =
            let
                oldStripeInfo =
                    userInfo.stripeInfo
            in
            { oldStripeInfo
                | paidUntil = Just paidUntil
            }
    }
