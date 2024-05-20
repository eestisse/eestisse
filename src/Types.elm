module Types exposing (..)

import Array exposing (Array)
import Auth.Common
import Background.Types as Background
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import EmailAddress
import EmailCode
import Http
import Lamdera exposing (ClientId, SessionId)
import Postmark exposing (PostmarkSendResponse)
import Responsive exposing (..)
import Route exposing (Route)
import Set exposing (Set)
import Stripe.Types as Stripe
import Time
import Time.Extra
import Translation.Types exposing (..)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , route : Route
    , time_bySecond : Time.Posix
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    , maybeAuthedUserInfo : Maybe (Maybe FrontendUserInfo) -- double maybe captures 3 states: not-yet-fetched, fetched and confirmed non-user, and confirmed user
    , signinModel : SigninModel
    , dProfile : Maybe DisplayProfile
    , maybeAdminData : Maybe AdminData
    , animationTime : Time.Posix
    , backgroundModel : Maybe Background.Model
    , maybePublicCreditsInfo : Maybe PublicCreditsInfo
    , cachedTranslationRecords : Dict Int TranslationRecord
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


type alias BackendModel =
    { time_bySecond : Time.Posix
    , publicCreditsInfo : PublicCreditsInfo
    , emails_backup : Set String
    , emailsWithConsents : List EmailAndConsents
    , preConsentRequests : List ( Time.Posix, String, Result GptAssistError Translation )
    , translationRecords : Array TranslationRecord
    , pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth
    , pendingEmailAuths : Dict String EmailCode.PendingEmailAuth
    , sessions : Dict Lamdera.SessionId SessionInfo
    , users : Dict Int UserInfo
    , nextUserId : Int
    , hangingInvoices : List PaidInvoice
    , secretCounter : Int
    , adminMessages : List ( Time.Posix, String )
    , lastAdminAlertEmailSent : Time.Posix
    , timeOfLastAdminMessageRead : Time.Posix
    }


type FrontendMsg
    = F_NoOp
    | UrlClicked UrlRequest
    | UrlChanged Url
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
    | StartGoogleSignin
    | StartEmailSignin
    | ChangeEmailForm EmailFormState
    | SubmitEmailForSignin EmailAddress.EmailAddress
    | SubmitEmailSigninCode EmailAddress.EmailAddress String
    | Logout
    | ChangeTranslationInput String
    | ChangePublicConsentChecked Bool
    | SubmitTextForTranslation Bool String
    | ShowBreakdown Int
    | CycleLoadingAnimation
    | GotoTranslateForm String
    | GotoRouteAndAnimate Route
    | GotoTranslate_FocusAndClear
    | Animate Time.Posix
    | FiddleRandomBackroundPath Time.Posix
    | StartStripePayment Int
    | UserIntent_ActivateMembership
    | UpdateFrontendNow_BySecond Time.Posix
    | ToggleMobileMenu
    | FetchMoreTranslations PublicOrPersonal ( Maybe Int, Int )
    | ChangeConsentsForm ConsentsFormModel
    | SubmitConsentsForm ConsentsFormModel
    | ChangeFeedbackForm FeedbackFormModel
    | SubmitFeedback Bool (Maybe String) String
    | MarkAdminMessagesRead Time.Posix


type BackendMsg
    = B_NoOp
    | AuthBackendMsg Auth.Common.BackendMsg
    | OnConnect SessionId ClientId
    | Daily
    | InitialTimeVal Time.Posix
    | GptResponseReceived ( SessionId, ClientId ) Bool String (Result Http.Error String)
    | AddPublicCredits
    | UpdateBackendNow_BySecond Time.Posix
    | SubscriptionDataReceived (Result Http.Error Stripe.SubscriptionData)
    | LoginCodeEmailSentResponse ( EmailAddress.EmailAddress, String ) (Result Http.Error PostmarkSendResponse)


type ToBackend
    = TB_NoOp
    | TB_AuthMsg Auth.Common.ToBackend
    | TB_TextForTranslation Bool String
    | R_AdminData
    | R_GeneralData
    | TB_Logout
    | R_TranslationRecords PublicOrPersonal ( Maybe Int, Int )
    | R_SingleTranslationRecord Int
    | TB_SetPostAuthRedirect Route.Route
    | R_AndClearRedirectReturnPage
    | R_EmailLoginCode EmailAddress.EmailAddress
    | TB_EmailSigninCode EmailAddress.EmailAddress String
    | TB_Consents ConsentsFormModel
    | TB_SetPublicTranslateChecked Bool
    | TB_UserFeedback Bool (Maybe String) String
    | TB_SetAdminMessagesLastRead Time.Posix


type ToFrontend
    = TF_NoOp
    | TF_AuthMsg Auth.Common.ToFrontend
    | TF_AuthSuccess FrontendUserInfo
    | TF_UserInfo (Maybe FrontendUserInfo)
    | TF_TranslationResult String (Result GptAssistError TranslationRecord)
    | TF_AdminData AdminData
    | TF_GeneralData GeneralData
    | TF_CreditsInfo PublicCreditsInfo
    | TF_TranslationRecordsRequestResult (Result TranslationRecordFetchError (List TranslationRecord))
    | TF_NoMoreTranslationsToFetch PublicOrPersonal
    | TF_RedirectReturnPage (Maybe Route.Route)
    | TF_LoginCodeError LoginCodeErr
    | TF_AckUserFeedback


type TranslationRecordFetchError
    = InvalidTranslationRecordId
    | IncorrectPermissionForTranslationRecord


trFetchErrorToString : TranslationRecordFetchError -> String
trFetchErrorToString err =
    case err of
        InvalidTranslationRecordId ->
            "Invalid translation ID"

        IncorrectPermissionForTranslationRecord ->
            "User doesn't have permission to view that record"


type LoginCodeErr
    = IncorrectCode
    | CodeExpired


type alias SessionInfo =
    { maybeAuthedUserId : Maybe Int
    , redirectReturnPage : Maybe Route.Route
    }


blankSession =
    SessionInfo Nothing Nothing


type alias DoTranslateModel =
    { input : String
    , state : DoTranslateState
    }


type DoTranslateState
    = Inputting
    | TranslateRequestSubmitted
    | Error GptAssistError


type alias ViewTranslationModel =
    { maybeSelectedBreakdownPartId : Maybe Int }


type alias PaidInvoice =
    { customerId : String
    , paidUntil : Time.Posix
    }


type alias UserInfo =
    { email : String
    , stripeInfo : Maybe StripeInfo
    , consents : Maybe UserConsents
    , publicChecked : Bool
    }


type alias UserConsents =
    { interview : Bool -- "a 30-minute user interview call in exchange for free credit"
    , features : Bool -- "an offer of free credit in exchange for a 30-minute user interview"
    }


type alias FrontendUserInfo =
    { id : Int
    , email : String
    , membershipStatus : MembershipStatus
    , consentsSubmitted : Bool
    , publicConsentChecked : Bool
    }


type alias StripeInfo =
    { customerId : String
    , subscriptionId : String
    , paidUntil : Maybe Time.Posix
    }


type alias AdminData =
    { emailsAndConsents : List ( String, Int )
    , adminMessages : List ( Time.Posix, String )
    , numPaidUsers : Int
    }


type alias CreditsCounterAnimationState =
    { goingUp : Bool
    , startTime : Time.Posix
    }


type alias GeneralData =
    { publicCreditsInfo : PublicCreditsInfo }


getGeneralDataFromModel : BackendModel -> GeneralData
getGeneralDataFromModel model =
    GeneralData model.publicCreditsInfo


type alias EmailAndConsents =
    { email : String
    , consentsGiven : List String
    }


type alias SignupFormModel =
    { emailInput : String
    , newFeaturesConsentChecked : Bool
    , userInterviewsConsentChecked : Bool
    }


type alias PublicCreditsInfo =
    { current : Int
    , nextRefresh : Time.Posix
    , refreshAmount : Int
    }


blankSignupForm : SignupFormModel
blankSignupForm =
    SignupFormModel
        ""
        False
        False


type RequestState
    = Waiting String Int
    | RequestComplete CompletedRequest


type alias CompletedRequest =
    { inputText : String
    , translationResult : Result GptAssistError Translation
    , maybeSelectedBreakdownPart : Maybe BreakdownPart
    }


type alias RGB =
    { red : Float
    , green : Float
    , blue : Float
    }


updateUserStripeInfo : StripeInfo -> UserInfo -> UserInfo
updateUserStripeInfo stripeInfo userInfo =
    { userInfo
        | stripeInfo = Just stripeInfo
    }


type MembershipStatus
    = NoStripeInfo
    | NotStarted
    | MembershipActive
    | MembershipAlmostExpired
    | MembershipExpired


toFrontendUserInfo : Int -> UserInfo -> Time.Posix -> FrontendUserInfo
toFrontendUserInfo id userInfo now =
    let
        membershipStatus =
            userMembershipStatus now userInfo
    in
    { id = id
    , email = userInfo.email
    , membershipStatus = membershipStatus
    , consentsSubmitted = userInfo.consents /= Nothing
    , publicConsentChecked = userInfo.publicChecked
    }


maybeFrontendUserSignupComplete : Maybe FrontendUserInfo -> Bool
maybeFrontendUserSignupComplete maybeFrontendUserInfo =
    case maybeFrontendUserInfo of
        Nothing ->
            False

        Just userInfo ->
            userInfo.consentsSubmitted
                && (case userInfo.membershipStatus of
                        MembershipActive ->
                            True

                        MembershipAlmostExpired ->
                            True

                        _ ->
                            False
                   )


getTranslationRecord : Int -> FrontendModel -> Maybe TranslationRecord
getTranslationRecord id model =
    Dict.get id model.cachedTranslationRecords


type alias SigninModel =
    { emailFormMode : EmailFormState
    }


type EmailFormState
    = Inactive
    | InputtingEmail String
    | InputtingCode InputtingCodeModel
    | CodeSubmitted EmailAddress.EmailAddress


type alias InputtingCodeModel =
    { emailAddress : EmailAddress.EmailAddress
    , input : String
    , maybeError : Maybe LoginCodeErr
    }


type alias ConsentsFormModel =
    { interview : Bool
    , features : Bool
    }


type alias FeedbackFormModel =
    { textInput : String
    , emailInput : String
    , submitStatus : SubmitStatus
    }


type SubmitStatus
    = NotSubmitted
    | SubmitWaiting
    | Complete


blankFeedbackFormModel =
    FeedbackFormModel "" "" NotSubmitted


userMembershipStatus : Time.Posix -> UserInfo -> MembershipStatus
userMembershipStatus nowish user =
    case user.stripeInfo of
        Nothing ->
            NoStripeInfo

        Just stripeInfo ->
            case stripeInfo.paidUntil of
                Nothing ->
                    NotStarted

                Just paidUntil ->
                    if Time.Extra.compare paidUntil nowish == GT then
                        MembershipActive

                    else if Time.Extra.diff Time.Extra.Day Time.utc paidUntil nowish <= 2 then
                        MembershipAlmostExpired

                    else
                        MembershipExpired
