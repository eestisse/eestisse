module Evergreen.V44.Types exposing (..)

import Array
import Browser
import Browser.Dom
import Browser.Navigation
import Dict
import Evergreen.V44.Auth.Common
import Evergreen.V44.Background.Types
import Evergreen.V44.EmailAddress
import Evergreen.V44.EmailCode
import Evergreen.V44.Postmark
import Evergreen.V44.Responsive
import Evergreen.V44.Route
import Evergreen.V44.Stripe.Types
import Evergreen.V44.Translation.Types
import Http
import Lamdera
import Set
import Time
import Url


type MembershipStatus
    = NoStripeInfo
    | NotStarted
    | MembershipActive
    | MembershipAlmostExpired
    | MembershipExpired


type alias FrontendUserInfo =
    { id : Int
    , email : String
    , membershipStatus : MembershipStatus
    , consentsSubmitted : Bool
    , publicConsentChecked : Bool
    }


type LoginCodeErr
    = IncorrectCode
    | CodeExpired


type alias InputtingCodeModel =
    { emailAddress : Evergreen.V44.EmailAddress.EmailAddress
    , input : String
    , maybeError : Maybe LoginCodeErr
    }


type EmailFormMode
    = Inactive
    | InputtingEmail String
    | InputtingCode InputtingCodeModel
    | CodeSubmitted Evergreen.V44.EmailAddress.EmailAddress


type alias SigninModel =
    { emailFormMode : EmailFormMode
    }


type alias AdminData =
    { emailsAndConsents : List ( String, Int )
    , adminMessages : List ( Time.Posix, String )
    }


type alias PublicCreditsInfo =
    { current : Int
    , nextRefresh : Time.Posix
    , refreshAmount : Int
    }


type alias CreditsCounterAnimationState =
    { goingUp : Bool
    , startTime : Time.Posix
    }


type DoTranslateState
    = Inputting
    | TranslateRequestSubmitted
    | Error Evergreen.V44.Translation.Types.GptAssistError


type alias DoTranslateModel =
    { input : String
    , state : DoTranslateState
    }


type alias ViewTranslationModel =
    { maybeSelectedBreakdownPartId : Maybe Int
    }


type alias ConsentsFormModel =
    { interview : Bool
    , features : Bool
    }


type SubmitStatus
    = NotSubmitted
    | SubmitWaiting
    | Complete


type alias FeedbackFormModel =
    { textInput : String
    , emailInput : String
    , submitStatus : SubmitStatus
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , route : Evergreen.V44.Route.Route
    , authFlow : Evergreen.V44.Auth.Common.Flow
    , authRedirectBaseUrl : Url.Url
    , maybeAuthedUserInfo : Maybe (Maybe FrontendUserInfo)
    , signinModel : SigninModel
    , dProfile : Maybe Evergreen.V44.Responsive.DisplayProfile
    , maybeAdminData : Maybe AdminData
    , animationTime : Time.Posix
    , time_updatePerSecond : Time.Posix
    , backgroundModel : Maybe Evergreen.V44.Background.Types.Model
    , maybePublicCreditsInfo : Maybe PublicCreditsInfo
    , showCreditCounterTooltip : Bool
    , creditsCounterAnimationState : Maybe CreditsCounterAnimationState
    , cachedTranslationRecords : Dict.Dict Int Evergreen.V44.Translation.Types.TranslationRecord
    , doTranslateModel : DoTranslateModel
    , publicConsentChecked : Bool
    , viewTranslationModel : ViewTranslationModel
    , loadingAnimationCounter : Int
    , mobileMenuOpen : Bool
    , noMorePublicTranslationsToFetch : Bool
    , noMorePersonalTranslationsToFetch : Bool
    , fetchingRecords : Bool
    , maybeConsentsFormModel : Maybe ConsentsFormModel
    , feedbackFormModel : FeedbackFormModel
    }


type alias EmailAndConsents =
    { email : String
    , consentsGiven : List String
    }


type alias SessionInfo =
    { maybeAuthedUserId : Maybe Int
    , redirectReturnPage : Maybe Evergreen.V44.Route.Route
    }


type alias StripeInfo =
    { customerId : String
    , subscriptionId : String
    , paidUntil : Maybe Time.Posix
    }


type alias UserConsents =
    { interview : Bool
    , features : Bool
    }


type alias UserInfo =
    { email : String
    , stripeInfo : Maybe StripeInfo
    , consents : Maybe UserConsents
    , publicChecked : Bool
    }


type alias PaidInvoice =
    { customerId : String
    , paidUntil : Time.Posix
    }


type alias BackendModel =
    { nowish : Time.Posix
    , publicCreditsInfo : PublicCreditsInfo
    , emails_backup : Set.Set String
    , emailsWithConsents : List EmailAndConsents
    , preConsentRequests : List ( Time.Posix, String, Result Evergreen.V44.Translation.Types.GptAssistError Evergreen.V44.Translation.Types.Translation )
    , translationRecords : Array.Array Evergreen.V44.Translation.Types.TranslationRecord
    , pendingAuths : Dict.Dict Lamdera.SessionId Evergreen.V44.Auth.Common.PendingAuth
    , pendingEmailAuths : Dict.Dict String Evergreen.V44.EmailCode.PendingEmailAuth
    , sessions : Dict.Dict Lamdera.SessionId SessionInfo
    , users : Dict.Dict Int UserInfo
    , nextUserId : Int
    , hangingInvoices : List PaidInvoice
    , secretCounter : Int
    , adminMessages : List ( Time.Posix, String )
    , lastAdminAlertEmailSent : Time.Posix
    , timeOfLastAdminMessageRead : Time.Posix
    }


type FrontendMsg
    = NoOpFrontendMsg
    | GoogleSigninRequested
    | EmailSigninRequested
    | ChangeEmailForm EmailFormMode
    | SendEmailToBackendForCode Evergreen.V44.EmailAddress.EmailAddress
    | SubmitCodeClicked Evergreen.V44.EmailAddress.EmailAddress String
    | Logout
    | UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
    | TranslationInputChanged String
    | PublicConsentChecked Bool
    | SubmitText Bool String
    | ShowExplanation Int
    | CycleLoadingAnimation
    | EditTranslation String
    | GotoRouteAndAnimate Evergreen.V44.Route.Route
    | GotoTranslate_FocusAndClear
    | FetchImportantNumber
    | Animate Time.Posix
    | FiddleRandomBackroundPath Time.Posix
    | ShowCreditCounterTooltip Bool
    | TriggerStripePayment Int
    | UserIntent_ActivateMembership
    | UpdateFrontendNow Time.Posix
    | ToggleMobileMenu
    | LoadMoreClicked Evergreen.V44.Translation.Types.PublicOrPersonal ( Maybe Int, Int )
    | ConsentsFormChanged ConsentsFormModel
    | ConsentsFormSubmitClicked ConsentsFormModel
    | FeedbackFormChanged FeedbackFormModel
    | TriggerSubmitFeedback Bool (Maybe String) String
    | MarkAdminMessagesRead Time.Posix


type ToBackend
    = NoOpToBackend
    | AuthToBackend Evergreen.V44.Auth.Common.ToBackend
    | SubmitTextForTranslation Bool String
    | RequestAdminData
    | RequestGeneralData
    | DoLogout
    | RequestTranslations Evergreen.V44.Translation.Types.PublicOrPersonal ( Maybe Int, Int )
    | RequestTranslation Int
    | SetPostAuthRedirect Evergreen.V44.Route.Route
    | RequestAndClearRedirectReturnPage
    | RequestEmailLoginCode Evergreen.V44.EmailAddress.EmailAddress
    | SubmitCodeForEmail Evergreen.V44.EmailAddress.EmailAddress String
    | SubmitConsentsForm ConsentsFormModel
    | PublicTranslateCheck Bool
    | UserFeedback Bool (Maybe String) String
    | MarkAdminMessagesReadToBackend Time.Posix


type BackendMsg
    = NoOpBackendMsg
    | Daily
    | InitialTimeVal Time.Posix
    | AuthBackendMsg Evergreen.V44.Auth.Common.BackendMsg
    | GptResponseReceived ( Lamdera.SessionId, Lamdera.ClientId ) Bool String (Result Http.Error String)
    | AddPublicCredits
    | UpdateBackendNow Time.Posix
    | OnConnect Lamdera.SessionId Lamdera.ClientId
    | SubscriptionDataReceived (Result Http.Error Evergreen.V44.Stripe.Types.SubscriptionData)
    | LoginCodeEmailSentResponse ( Evergreen.V44.EmailAddress.EmailAddress, String ) (Result Http.Error Evergreen.V44.Postmark.PostmarkSendResponse)


type alias GeneralData =
    { publicCreditsInfo : PublicCreditsInfo
    }


type TranslationRecordFetchError
    = InvalidTranslationRecordId
    | IncorrectPermissionForTranslationRecord


type ToFrontend
    = NoOpToFrontend
    | AuthToFrontend Evergreen.V44.Auth.Common.ToFrontend
    | AuthSuccess FrontendUserInfo
    | UpdateUserInfo (Maybe FrontendUserInfo)
    | TranslationResult String (Result Evergreen.V44.Translation.Types.GptAssistError Evergreen.V44.Translation.Types.TranslationRecord)
    | AdminDataMsg AdminData
    | GeneralDataMsg GeneralData
    | CreditsInfoUpdated PublicCreditsInfo
    | RequestTranslationRecordsResult (Result TranslationRecordFetchError (List Evergreen.V44.Translation.Types.TranslationRecord))
    | NoMoreTranslationsToFetch Evergreen.V44.Translation.Types.PublicOrPersonal
    | RequestRedirectReturnPageResult (Maybe Evergreen.V44.Route.Route)
    | LoginCodeError LoginCodeErr
    | AckUserFeedback
