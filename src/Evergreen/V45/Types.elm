module Evergreen.V45.Types exposing (..)

import Array
import Browser
import Browser.Dom
import Browser.Navigation
import Dict
import Evergreen.V45.Auth.Common
import Evergreen.V45.Background.Types
import Evergreen.V45.EmailAddress
import Evergreen.V45.EmailCode
import Evergreen.V45.Postmark
import Evergreen.V45.Responsive
import Evergreen.V45.Route
import Evergreen.V45.Stripe.Types
import Evergreen.V45.Translation.Types
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
    { emailAddress : Evergreen.V45.EmailAddress.EmailAddress
    , input : String
    , maybeError : Maybe LoginCodeErr
    }


type EmailFormState
    = Inactive
    | InputtingEmail String
    | InputtingCode InputtingCodeModel
    | CodeSubmitted Evergreen.V45.EmailAddress.EmailAddress


type alias SigninModel =
    { emailFormMode : EmailFormState
    }


type alias EmailAndConsents =
    { email : String
    , consentsGiven : List String
    }


type alias AdminData =
    { emailsAndConsents : List EmailAndConsents
    , adminMessages : List ( Time.Posix, String )
    , numPaidUsers : Int
    }


type alias PublicCreditsInfo =
    { current : Int
    , nextRefresh : Time.Posix
    , refreshAmount : Int
    }


type DoTranslateState
    = Inputting
    | TranslateRequestSubmitted
    | Error Evergreen.V45.Translation.Types.GptAssistError


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
    , route : Evergreen.V45.Route.Route
    , time_bySecond : Time.Posix
    , authFlow : Evergreen.V45.Auth.Common.Flow
    , authRedirectBaseUrl : Url.Url
    , maybeAuthedUserInfo : Maybe (Maybe FrontendUserInfo)
    , signinModel : SigninModel
    , dProfile : Maybe Evergreen.V45.Responsive.DisplayProfile
    , maybeAdminData : Maybe AdminData
    , animationTime : Time.Posix
    , backgroundModel : Maybe Evergreen.V45.Background.Types.Model
    , maybePublicCreditsInfo : Maybe PublicCreditsInfo
    , cachedTranslationRecords : Dict.Dict Int Evergreen.V45.Translation.Types.TranslationRecord
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


type alias SessionInfo =
    { maybeAuthedUserId : Maybe Int
    , redirectReturnPage : Maybe Evergreen.V45.Route.Route
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
    { time_bySecond : Time.Posix
    , publicCreditsInfo : PublicCreditsInfo
    , emails_backup : Set.Set String
    , emailsWithConsents : List EmailAndConsents
    , preConsentRequests : List ( Time.Posix, String, Result Evergreen.V45.Translation.Types.GptAssistError Evergreen.V45.Translation.Types.Translation )
    , translationRecords : Array.Array Evergreen.V45.Translation.Types.TranslationRecord
    , pendingAuths : Dict.Dict Lamdera.SessionId Evergreen.V45.Auth.Common.PendingAuth
    , pendingEmailAuths : Dict.Dict String Evergreen.V45.EmailCode.PendingEmailAuth
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
    = F_NoOp
    | UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
    | StartGoogleSignin
    | StartEmailSignin
    | ChangeEmailForm EmailFormState
    | SubmitEmailForSignin Evergreen.V45.EmailAddress.EmailAddress
    | SubmitEmailSigninCode Evergreen.V45.EmailAddress.EmailAddress String
    | Logout
    | ChangeTranslationInput String
    | ChangePublicConsentChecked Bool
    | SubmitTextForTranslation Bool String
    | ShowBreakdown Int
    | CycleLoadingAnimation
    | GotoTranslateForm String
    | GotoRouteAndAnimate Evergreen.V45.Route.Route
    | GotoTranslate_FocusAndClear
    | Animate Time.Posix
    | FiddleRandomBackroundPath Time.Posix
    | StartStripePayment Int
    | UserIntent_ActivateMembership
    | UpdateFrontendNow_BySecond Time.Posix
    | ToggleMobileMenu
    | FetchMoreTranslations Evergreen.V45.Translation.Types.PublicOrPersonal ( Maybe Int, Int )
    | ChangeConsentsForm ConsentsFormModel
    | SubmitConsentsForm ConsentsFormModel
    | ChangeFeedbackForm FeedbackFormModel
    | SubmitFeedback Bool (Maybe String) String
    | MarkAdminMessagesRead Time.Posix
    | TestAdminError


type ToBackend
    = TB_NoOp
    | TB_AuthMsg Evergreen.V45.Auth.Common.ToBackend
    | TB_TextForTranslation Bool String
    | R_AdminData
    | R_GeneralData
    | TB_Logout
    | R_TranslationRecords Evergreen.V45.Translation.Types.PublicOrPersonal ( Maybe Int, Int )
    | R_SingleTranslationRecord Int
    | TB_SetPostAuthRedirect Evergreen.V45.Route.Route
    | R_AndClearRedirectReturnPage
    | R_EmailLoginCode Evergreen.V45.EmailAddress.EmailAddress
    | TB_EmailSigninCode Evergreen.V45.EmailAddress.EmailAddress String
    | TB_Consents ConsentsFormModel
    | TB_SetPublicTranslateChecked Bool
    | TB_UserFeedback Bool (Maybe String) String
    | TB_SetAdminMessagesLastRead Time.Posix
    | TB_TestAdminError String


type BackendMsg
    = B_NoOp
    | AuthBackendMsg Evergreen.V45.Auth.Common.BackendMsg
    | OnConnect Lamdera.SessionId Lamdera.ClientId
    | Daily
    | InitialTimeVal Time.Posix
    | GptResponseReceived ( Lamdera.SessionId, Lamdera.ClientId ) Bool String (Result Http.Error String)
    | AddPublicCredits
    | UpdateBackendNow_BySecond Time.Posix
    | SubscriptionDataReceived (Result Http.Error Evergreen.V45.Stripe.Types.SubscriptionData)
    | LoginCodeEmailSentResponse ( Evergreen.V45.EmailAddress.EmailAddress, String ) (Result Http.Error Evergreen.V45.Postmark.PostmarkSendResponse)


type alias GeneralData =
    { publicCreditsInfo : PublicCreditsInfo
    }


type TranslationRecordFetchError
    = InvalidTranslationRecordId
    | IncorrectPermissionForTranslationRecord


type ToFrontend
    = TF_NoOp
    | TF_AuthMsg Evergreen.V45.Auth.Common.ToFrontend
    | TF_AuthSuccess FrontendUserInfo
    | TF_UserInfo (Maybe FrontendUserInfo)
    | TF_TranslationResult String (Result Evergreen.V45.Translation.Types.GptAssistError Evergreen.V45.Translation.Types.TranslationRecord)
    | TF_AdminData AdminData
    | TF_GeneralData GeneralData
    | TF_CreditsInfo PublicCreditsInfo
    | TF_TranslationRecordsRequestResult (Result TranslationRecordFetchError (List Evergreen.V45.Translation.Types.TranslationRecord))
    | TF_NoMoreTranslationsToFetch Evergreen.V45.Translation.Types.PublicOrPersonal
    | TF_RedirectReturnPage (Maybe Evergreen.V45.Route.Route)
    | TF_LoginCodeError LoginCodeErr
    | TF_AckUserFeedback
