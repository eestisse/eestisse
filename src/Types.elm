module Types exposing (..)

import Array exposing (Array)
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
import Translation.Types exposing (..)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , route : Route
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    , maybeAuthedUserInfo : Maybe FrontendUserInfo
    , dProfile : Maybe DisplayProfile
    , signupState : SignupState
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
    }


type alias BackendModel =
    { nowish : Time.Posix
    , publicCreditsInfo : PublicCreditsInfo
    , emails_backup : Set String
    , emailsWithConsents : List EmailAndConsents
    , preConsentRequests : List ( Time.Posix, String, Result GptAssistError Translation )
    , translationRecords : Array TranslationRecord
    , pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth
    , authedSessions : Dict Lamdera.SessionId Int
    , users : Dict Int UserInfo
    , nextUserId : Int
    , hangingInvoices : List PaidInvoice
    }


type FrontendMsg
    = NoOpFrontendMsg
    | AuthSigninRequested { methodId : Auth.Common.MethodId, username : Maybe String }
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
    | StartSignup
    | SubmitSignupClicked SignupFormModel
    | SignupFormChanged SignupFormModel
    | FetchImportantNumber
    | Animate Time.Posix
    | FiddleRandomBackroundPath Time.Posix
    | ShowCreditCounterTooltip Bool
    | TriggerStripePayment Int
    | UserIntent_ActivateMembership
    | UpdateFrontendNow Time.Posix


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
    | SubmitSignup SignupFormModel
    | RequestImportantNumber
    | RequestGeneralData
    | DoLogout
    | RequestPublicTranslations
    | RequestTranslation Int


type ToFrontend
    = NoOpToFrontend
    | AuthToFrontend Auth.Common.ToFrontend
    | AuthSuccess FrontendUserInfo
    | TranslationResult String (Result GptAssistError TranslationRecord)
    | EmailSubmitAck
    | AdminDataMsg AdminData
    | GeneralDataMsg GeneralData
    | CreditsInfoUpdated PublicCreditsInfo
    | RequestTranslationRecordsResult (Result String (List TranslationRecord))


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
