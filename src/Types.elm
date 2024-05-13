module Types exposing (..)

import Array exposing (Array)
import Auth.Common
import Background.Types as Background
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import EmailCode
import Http
import Lamdera exposing (ClientId, SessionId)
import Responsive exposing (..)
import Route exposing (Route)
import Set exposing (Set)
import Stripe.Types as Stripe
import Time
import Translation.Types exposing (..)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , route : Route
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    , maybeAuthedUserInfo : Maybe FrontendUserInfo
    , signinModel : SigninModel
    , dProfile : Maybe DisplayProfile
    , maybeAdminData : Maybe AdminData
    , animationTime : Time.Posix
    , time_updatePerSecond : Time.Posix
    , backgroundModel : Maybe Background.Model
    , maybePublicCreditsInfo : Maybe PublicCreditsInfo
    , showCreditCounterTooltip : Bool
    , creditsCounterAnimationState : Maybe CreditsCounterAnimationState
    , cachedTranslationRecords : Dict Int TranslationRecord
    , doTranslateModel : DoTranslateModel
    , publicConsentChecked : Bool
    , viewTranslationModel : ViewTranslationModel
    , loadingAnimationCounter : Int
    , mobileMenuOpen : Bool
    , noMorePublicTranslationsToFetch : Bool
    , noMorePersonalTranslationsToFetch : Bool
    , fetchingRecords : Bool
    }


type alias BackendModel =
    { nowish : Time.Posix
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
    }


type FrontendMsg
    = NoOpFrontendMsg
    | GoogleSigninRequested
    | EmailSigninRequested
    | ChangeEmailForm EmailFormMode
    | SubmitEmailClicked String
    | SubmitCodeClicked String String
    | Logout
    | UrlClicked UrlRequest
    | UrlChanged Url
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
    | TranslationInputChanged String
    | PublicConsentChecked Bool
    | SubmitText Bool String
    | ShowExplanation Int
    | CycleLoadingAnimation
    | EditTranslation String
    | GotoRouteAndAnimate Route
    | GotoTranslate_FocusAndClear
    | FetchImportantNumber
    | Animate Time.Posix
    | FiddleRandomBackroundPath Time.Posix
    | ShowCreditCounterTooltip Bool
    | TriggerStripePayment Int
    | UserIntent_ActivateMembership
    | UpdateFrontendNow Time.Posix
    | ToggleMobileMenu
    | LoadMoreClicked PublicOrPersonal ( Maybe Int, Int )


type BackendMsg
    = NoOpBackendMsg
    | InitialTimeVal Time.Posix
    | AuthBackendMsg Auth.Common.BackendMsg
    | GptResponseReceived ( SessionId, ClientId ) Bool String (Result Http.Error String)
    | AddPublicCredits
    | UpdateBackendNow Time.Posix
    | OnConnect SessionId ClientId
    | SubscriptionDataReceived (Result Http.Error Stripe.SubscriptionData)


type ToBackend
    = NoOpToBackend
    | AuthToBackend Auth.Common.ToBackend
    | SubmitTextForTranslation Bool String
    | RequestImportantNumber
    | RequestGeneralData
    | DoLogout
    | RequestTranslations PublicOrPersonal ( Maybe Int, Int )
    | RequestTranslation Int
    | SetPostAuthRedirect Route.Route
    | RequestAndClearRedirectReturnPage
    | RequestEmailLoginCode String
    | SubmitCodeForEmail String String


type ToFrontend
    = NoOpToFrontend
    | AuthToFrontend Auth.Common.ToFrontend
    | AuthSuccess FrontendUserInfo
    | TranslationResult String (Result GptAssistError TranslationRecord)
    | AdminDataMsg AdminData
    | GeneralDataMsg GeneralData
    | CreditsInfoUpdated PublicCreditsInfo
    | RequestTranslationRecordsResult (Result String (List TranslationRecord))
    | NoMoreTranslationsToFetch PublicOrPersonal
    | RequestRedirectReturnPageResult (Maybe Route.Route)
    | LogoutAck


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
    }


type alias FrontendUserInfo =
    { id : Int
    , email : String
    , membershipStatus : MembershipStatus
    }


type alias StripeInfo =
    { customerId : String
    , subscriptionId : String
    , paidUntil : Maybe Time.Posix
    }


type alias AdminData =
    { emailsAndConsents : List ( String, Int )
    }


type alias CreditsCounterAnimationState =
    { goingUp : Bool
    , startTime : Time.Posix
    }


type alias GeneralData =
    { publicCreditsInfo : PublicCreditsInfo }


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


toFrontendUserInfo : ( Int, UserInfo, MembershipStatus ) -> FrontendUserInfo
toFrontendUserInfo ( id, userInfo, membershipStatus ) =
    { id = id
    , email = userInfo.email
    , membershipStatus = membershipStatus
    }


maybeFrontendUserHasActiveMembership : Maybe FrontendUserInfo -> Bool
maybeFrontendUserHasActiveMembership maybeFrontendUserInfo =
    case maybeFrontendUserInfo of
        Nothing ->
            False

        Just userInfo ->
            case userInfo.membershipStatus of
                MembershipActive ->
                    True

                MembershipAlmostExpired ->
                    True

                _ ->
                    False


getTranslationRecord : Int -> FrontendModel -> Maybe TranslationRecord
getTranslationRecord id model =
    Dict.get id model.cachedTranslationRecords


type alias SigninModel =
    { emailFormMode : EmailFormMode
    }


type EmailFormMode
    = Inactive
    | InputtingEmail String
    | InputtingCode String String
    | CodeSubmitted
