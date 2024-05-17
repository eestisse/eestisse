module Evergreen.Migrate.V44 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Array
import Config
import Dict
import Evergreen.V43.Auth.Common
import Evergreen.V43.Background.Types
import Evergreen.V43.OAuth
import Evergreen.V43.OAuth.AuthorizationCode
import Evergreen.V43.Point
import Evergreen.V43.Responsive
import Evergreen.V43.Route
import Evergreen.V43.Types
import Evergreen.V44.Auth.Common
import Evergreen.V44.Background.Types
import Evergreen.V44.OAuth
import Evergreen.V44.OAuth.AuthorizationCode
import Evergreen.V44.Point
import Evergreen.V44.Responsive
import Evergreen.V44.Route
import Evergreen.V44.Translation.Types
import Evergreen.V44.Types
import Lamdera.Migrations exposing (..)
import List
import Maybe
import Time


frontendModel : Evergreen.V43.Types.FrontendModel -> ModelMigration Evergreen.V44.Types.FrontendModel Evergreen.V44.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V43.Types.BackendModel -> ModelMigration Evergreen.V44.Types.BackendModel Evergreen.V44.Types.BackendMsg
backendModel old =
    ModelMigrated ( migrate_Types_BackendModel old, Cmd.none )


frontendMsg : Evergreen.V43.Types.FrontendMsg -> MsgMigration Evergreen.V44.Types.FrontendMsg Evergreen.V44.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V43.Types.ToBackend -> MsgMigration Evergreen.V44.Types.ToBackend Evergreen.V44.Types.BackendMsg
toBackend old =
    MsgMigrated ( migrate_Types_ToBackend old, Cmd.none )


backendMsg : Evergreen.V43.Types.BackendMsg -> MsgMigration Evergreen.V44.Types.BackendMsg Evergreen.V44.Types.BackendMsg
backendMsg old =
    MsgMigrated ( migrate_Types_BackendMsg old, Cmd.none )


toFrontend : Evergreen.V43.Types.ToFrontend -> MsgMigration Evergreen.V44.Types.ToFrontend Evergreen.V44.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_BackendModel : Evergreen.V43.Types.BackendModel -> Evergreen.V44.Types.BackendModel
migrate_Types_BackendModel old =
    let
        preConsentRequests : List ( Time.Posix, String, Result Evergreen.V44.Translation.Types.GptAssistError Evergreen.V44.Translation.Types.Translation )
        preConsentRequests =
            old.requests
                |> List.filterMap
                    (\( time, ( input, public ), result ) ->
                        if not public then
                            Just
                                ( time
                                , input
                                , result
                                    |> Result.map migrate_Translation_Types_Translation
                                    |> Result.mapError migrate_Translation_Types_GptAssistError
                                )

                        else
                            Nothing
                    )

        translationRecords : Array.Array Evergreen.V44.Translation.Types.TranslationRecord
        translationRecords =
            old.requests
                |> List.filterMap
                    (\( time, ( input, public ), result ) ->
                        if public then
                            case result of
                                Ok translation ->
                                    Just ( time, input, translation )

                                Err _ ->
                                    Nothing

                        else
                            Nothing
                    )
                |> List.indexedMap
                    (\id ( time, input, translation ) ->
                        { id = id
                        , fromUserId = Nothing
                        , public = True
                        , time = time
                        , input = input
                        , translation = translation |> migrate_Translation_Types_Translation
                        }
                    )
                |> Array.fromList
    in
    { nowish = old.nowish
    , publicCreditsInfo =
        { current = 10
        , nextRefresh =
            Time.posixToMillis old.nowish
                + Config.publicUsageConfig.addCreditIntervalMillis
                |> Time.millisToPosix
        , refreshAmount = Config.publicUsageConfig.addCreditAmount
        }
    , emails_backup = old.emails_backup
    , emailsWithConsents = old.emailsWithConsents
    , preConsentRequests = preConsentRequests
    , translationRecords = translationRecords
    , pendingAuths = old.pendingAuths
    , pendingEmailAuths = Dict.empty
    , sessions = old.sessions |> Dict.map (\k -> migrate_Types_SessionInfo)
    , users = Dict.empty
    , nextUserId = 0
    , hangingInvoices = []
    , secretCounter = 0
    , adminMessages = []
    , lastAdminAlertEmailSent = Time.millisToPosix 0
    , timeOfLastAdminMessageRead = Time.millisToPosix 0
    }


migrate_Types_FrontendModel : Evergreen.V43.Types.FrontendModel -> Evergreen.V44.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , route = old.route |> migrate_Route_Route
    , authFlow = old.authFlow |> migrate_Auth_Common_Flow
    , authRedirectBaseUrl = old.authRedirectBaseUrl
    , maybeAuthedUserInfo = Nothing
    , signinModel = { emailFormMode = Evergreen.V44.Types.Inactive }
    , dProfile = old.dProfile |> Maybe.map migrate_Responsive_DisplayProfile
    , maybeAdminData = old.maybeAdminData |> Maybe.map migrate_Types_AdminData
    , animationTime = old.animationTime
    , time_updatePerSecond = Time.millisToPosix 0
    , backgroundModel = old.backgroundModel |> Maybe.map migrate_Background_Types_Model
    , maybePublicCreditsInfo = Nothing
    , showCreditCounterTooltip = old.showCreditCounterTooltip
    , creditsCounterAnimationState = old.creditsCounterAnimationState
    , cachedTranslationRecords = Dict.empty
    , doTranslateModel = { input = "", state = Evergreen.V44.Types.Inputting }
    , publicConsentChecked = old.publicConsentChecked
    , viewTranslationModel = { maybeSelectedBreakdownPartId = Nothing }
    , loadingAnimationCounter = 0
    , mobileMenuOpen = False
    , noMorePublicTranslationsToFetch = False
    , noMorePersonalTranslationsToFetch = False
    , fetchingRecords = False
    , maybeConsentsFormModel = Nothing
    , feedbackFormModel = Evergreen.V44.Types.FeedbackFormModel "" "" Evergreen.V44.Types.NotSubmitted
    }


migrate_Auth_Common_AuthChallengeReason : Evergreen.V43.Auth.Common.AuthChallengeReason -> Evergreen.V44.Auth.Common.AuthChallengeReason
migrate_Auth_Common_AuthChallengeReason old =
    case old of
        Evergreen.V43.Auth.Common.AuthSessionMissing ->
            Evergreen.V44.Auth.Common.AuthSessionMissing

        Evergreen.V43.Auth.Common.AuthSessionInvalid ->
            Evergreen.V44.Auth.Common.AuthSessionInvalid

        Evergreen.V43.Auth.Common.AuthSessionExpired ->
            Evergreen.V44.Auth.Common.AuthSessionExpired

        Evergreen.V43.Auth.Common.AuthSessionLoggedOut ->
            Evergreen.V44.Auth.Common.AuthSessionLoggedOut


migrate_Auth_Common_BackendMsg : Evergreen.V43.Auth.Common.BackendMsg -> Evergreen.V44.Auth.Common.BackendMsg
migrate_Auth_Common_BackendMsg old =
    case old of
        Evergreen.V43.Auth.Common.AuthSigninInitiated_ p0 ->
            Evergreen.V44.Auth.Common.AuthSigninInitiated_ p0

        Evergreen.V43.Auth.Common.AuthSigninInitiatedDelayed_ p0 p1 ->
            Evergreen.V44.Auth.Common.AuthSigninInitiatedDelayed_ p0 (p1 |> migrate_Auth_Common_ToFrontend)

        Evergreen.V43.Auth.Common.AuthCallbackReceived_ p0 p1 p2 p3 p4 p5 p6 ->
            Evergreen.V44.Auth.Common.AuthCallbackReceived_ p0 p1 p2 p3 p4 p5 p6

        Evergreen.V43.Auth.Common.AuthSuccess p0 p1 p2 p3 p4 ->
            Evergreen.V44.Auth.Common.AuthSuccess p0
                p1
                p2
                p3
                (p4 |> Result.mapError migrate_Auth_Common_Error >> Result.map (Tuple.mapBoth migrate_Auth_Common_UserInfo (Maybe.map migrate_Auth_Common_Token)))

        Evergreen.V43.Auth.Common.AuthRenewSession p0 p1 ->
            Evergreen.V44.Auth.Common.AuthRenewSession p0 p1

        Evergreen.V43.Auth.Common.AuthLogout p0 p1 ->
            Evergreen.V44.Auth.Common.AuthLogout p0 p1


migrate_Auth_Common_Error : Evergreen.V43.Auth.Common.Error -> Evergreen.V44.Auth.Common.Error
migrate_Auth_Common_Error old =
    case old of
        Evergreen.V43.Auth.Common.ErrStateMismatch ->
            Evergreen.V44.Auth.Common.ErrStateMismatch

        Evergreen.V43.Auth.Common.ErrAuthorization p0 ->
            Evergreen.V44.Auth.Common.ErrAuthorization (p0 |> migrate_OAuth_AuthorizationCode_AuthorizationError)

        Evergreen.V43.Auth.Common.ErrAuthentication p0 ->
            Evergreen.V44.Auth.Common.ErrAuthentication (p0 |> migrate_OAuth_AuthorizationCode_AuthenticationError)

        Evergreen.V43.Auth.Common.ErrHTTPGetAccessToken ->
            Evergreen.V44.Auth.Common.ErrHTTPGetAccessToken

        Evergreen.V43.Auth.Common.ErrHTTPGetUserInfo ->
            Evergreen.V44.Auth.Common.ErrHTTPGetUserInfo

        Evergreen.V43.Auth.Common.ErrAuthString p0 ->
            Evergreen.V44.Auth.Common.ErrAuthString p0


migrate_Auth_Common_Flow : Evergreen.V43.Auth.Common.Flow -> Evergreen.V44.Auth.Common.Flow
migrate_Auth_Common_Flow old =
    case old of
        Evergreen.V43.Auth.Common.Idle ->
            Evergreen.V44.Auth.Common.Idle

        Evergreen.V43.Auth.Common.Requested p0 ->
            Evergreen.V44.Auth.Common.Requested p0

        Evergreen.V43.Auth.Common.Pending ->
            Evergreen.V44.Auth.Common.Pending

        Evergreen.V43.Auth.Common.Authorized p0 p1 ->
            Evergreen.V44.Auth.Common.Authorized p0 p1

        Evergreen.V43.Auth.Common.Authenticated p0 ->
            Evergreen.V44.Auth.Common.Authenticated (p0 |> migrate_OAuth_Token)

        Evergreen.V43.Auth.Common.Done p0 ->
            Evergreen.V44.Auth.Common.Done (p0 |> migrate_Auth_Common_UserInfo)

        Evergreen.V43.Auth.Common.Errored p0 ->
            Evergreen.V44.Auth.Common.Errored (p0 |> migrate_Auth_Common_Error)


migrate_Auth_Common_ToBackend : Evergreen.V43.Auth.Common.ToBackend -> Evergreen.V44.Auth.Common.ToBackend
migrate_Auth_Common_ToBackend old =
    case old of
        Evergreen.V43.Auth.Common.AuthSigninInitiated p0 ->
            Evergreen.V44.Auth.Common.AuthSigninInitiated p0

        Evergreen.V43.Auth.Common.AuthCallbackReceived p0 p1 p2 p3 ->
            Evergreen.V44.Auth.Common.AuthCallbackReceived p0 p1 p2 p3

        Evergreen.V43.Auth.Common.AuthRenewSessionRequested ->
            Evergreen.V44.Auth.Common.AuthRenewSessionRequested

        Evergreen.V43.Auth.Common.AuthLogoutRequested ->
            Evergreen.V44.Auth.Common.AuthLogoutRequested


migrate_Auth_Common_ToFrontend : Evergreen.V43.Auth.Common.ToFrontend -> Evergreen.V44.Auth.Common.ToFrontend
migrate_Auth_Common_ToFrontend old =
    case old of
        Evergreen.V43.Auth.Common.AuthInitiateSignin p0 ->
            Evergreen.V44.Auth.Common.AuthInitiateSignin p0

        Evergreen.V43.Auth.Common.AuthError p0 ->
            Evergreen.V44.Auth.Common.AuthError (p0 |> migrate_Auth_Common_Error)

        Evergreen.V43.Auth.Common.AuthSessionChallenge p0 ->
            Evergreen.V44.Auth.Common.AuthSessionChallenge (p0 |> migrate_Auth_Common_AuthChallengeReason)


migrate_Auth_Common_Token : Evergreen.V43.Auth.Common.Token -> Evergreen.V44.Auth.Common.Token
migrate_Auth_Common_Token old =
    { methodId = old.methodId
    , token = old.token |> migrate_OAuth_Token
    , created = old.created
    , expires = old.expires
    }


migrate_Auth_Common_UserInfo : Evergreen.V43.Auth.Common.UserInfo -> Evergreen.V44.Auth.Common.UserInfo
migrate_Auth_Common_UserInfo old =
    old


migrate_Background_Types_Model : Evergreen.V43.Background.Types.Model -> Evergreen.V44.Background.Types.Model
migrate_Background_Types_Model old =
    { seed = old.seed
    , animationTime = old.animationTime
    , pathsAcross = old.pathsAcross |> List.map (Tuple.mapBoth migrate_Background_Types_PathAcross (Maybe.map migrate_Background_Types_PathAcrossAnimationState))
    }


migrate_Background_Types_PathAcross : Evergreen.V43.Background.Types.PathAcross -> Evergreen.V44.Background.Types.PathAcross
migrate_Background_Types_PathAcross old =
    { yPathStart = old.yPathStart
    , sections = old.sections |> List.map migrate_Background_Types_PathSection
    , color =
        old.color
            |> (\rec -> rec)
    }


migrate_Background_Types_PathAcrossAnimationState : Evergreen.V43.Background.Types.PathAcrossAnimationState -> Evergreen.V44.Background.Types.PathAcrossAnimationState
migrate_Background_Types_PathAcrossAnimationState old =
    { pathAcrossTarget = old.pathAcrossTarget |> migrate_Background_Types_PathAcross
    , animationStart = old.animationStart
    }


migrate_Background_Types_PathPiece : Evergreen.V43.Background.Types.PathPiece -> Evergreen.V44.Background.Types.PathPiece
migrate_Background_Types_PathPiece old =
    case old of
        Evergreen.V43.Background.Types.ElbowLeftToUp ->
            Evergreen.V44.Background.Types.ElbowLeftToUp

        Evergreen.V43.Background.Types.ElbowLeftToDown ->
            Evergreen.V44.Background.Types.ElbowLeftToDown

        Evergreen.V43.Background.Types.ElbowUpToRight ->
            Evergreen.V44.Background.Types.ElbowUpToRight

        Evergreen.V43.Background.Types.ElbowDownToRight ->
            Evergreen.V44.Background.Types.ElbowDownToRight

        Evergreen.V43.Background.Types.Right p0 ->
            Evergreen.V44.Background.Types.Right p0

        Evergreen.V43.Background.Types.Up p0 ->
            Evergreen.V44.Background.Types.Up p0

        Evergreen.V43.Background.Types.Down p0 ->
            Evergreen.V44.Background.Types.Down p0


migrate_Background_Types_PathSection : Evergreen.V43.Background.Types.PathSection -> Evergreen.V44.Background.Types.PathSection
migrate_Background_Types_PathSection old =
    { piece = old.piece |> migrate_Background_Types_PathPiece
    , endPointRelative = old.endPointRelative |> migrate_Point_Point
    , startPointRelative = old.startPointRelative |> migrate_Point_Point
    }


migrate_OAuth_AuthorizationCode_AuthenticationError : Evergreen.V43.OAuth.AuthorizationCode.AuthenticationError -> Evergreen.V44.OAuth.AuthorizationCode.AuthenticationError
migrate_OAuth_AuthorizationCode_AuthenticationError old =
    { error = old.error |> migrate_OAuth_ErrorCode
    , errorDescription = old.errorDescription
    , errorUri = old.errorUri
    }


migrate_OAuth_AuthorizationCode_AuthorizationError : Evergreen.V43.OAuth.AuthorizationCode.AuthorizationError -> Evergreen.V44.OAuth.AuthorizationCode.AuthorizationError
migrate_OAuth_AuthorizationCode_AuthorizationError old =
    { error = old.error |> migrate_OAuth_ErrorCode
    , errorDescription = old.errorDescription
    , errorUri = old.errorUri
    , state = old.state
    }


migrate_OAuth_ErrorCode : Evergreen.V43.OAuth.ErrorCode -> Evergreen.V44.OAuth.ErrorCode
migrate_OAuth_ErrorCode old =
    case old of
        Evergreen.V43.OAuth.InvalidRequest ->
            Evergreen.V44.OAuth.InvalidRequest

        Evergreen.V43.OAuth.UnauthorizedClient ->
            Evergreen.V44.OAuth.UnauthorizedClient

        Evergreen.V43.OAuth.AccessDenied ->
            Evergreen.V44.OAuth.AccessDenied

        Evergreen.V43.OAuth.UnsupportedResponseType ->
            Evergreen.V44.OAuth.UnsupportedResponseType

        Evergreen.V43.OAuth.InvalidScope ->
            Evergreen.V44.OAuth.InvalidScope

        Evergreen.V43.OAuth.ServerError ->
            Evergreen.V44.OAuth.ServerError

        Evergreen.V43.OAuth.TemporarilyUnavailable ->
            Evergreen.V44.OAuth.TemporarilyUnavailable

        Evergreen.V43.OAuth.Custom p0 ->
            Evergreen.V44.OAuth.Custom p0


migrate_OAuth_Token : Evergreen.V43.OAuth.Token -> Evergreen.V44.OAuth.Token
migrate_OAuth_Token old =
    case old of
        Evergreen.V43.OAuth.Bearer p0 ->
            Evergreen.V44.OAuth.Bearer p0


migrate_Point_Point : Evergreen.V43.Point.Point -> Evergreen.V44.Point.Point
migrate_Point_Point old =
    old


migrate_Responsive_DisplayProfile : Evergreen.V43.Responsive.DisplayProfile -> Evergreen.V44.Responsive.DisplayProfile
migrate_Responsive_DisplayProfile old =
    case old of
        Evergreen.V43.Responsive.Desktop ->
            Evergreen.V44.Responsive.Desktop

        Evergreen.V43.Responsive.Mobile ->
            Evergreen.V44.Responsive.Mobile


migrate_Route_Route : Evergreen.V43.Route.Route -> Evergreen.V44.Route.Route
migrate_Route_Route old =
    case old of
        Evergreen.V43.Route.Translate ->
            Evergreen.V44.Route.Translate

        Evergreen.V43.Route.Landing ->
            Evergreen.V44.Route.Landing

        Evergreen.V43.Route.Admin ->
            Evergreen.V44.Route.Admin

        Evergreen.V43.Route.Auth p0 ->
            Evergreen.V44.Route.BadRoute "auth"

        Evergreen.V43.Route.BadRoute ->
            Evergreen.V44.Route.BadRoute ""


migrate_Translation_Types_GptAssistError : Evergreen.V43.Types.GptAssistError -> Evergreen.V44.Translation.Types.GptAssistError
migrate_Translation_Types_GptAssistError old =
    case old of
        Evergreen.V43.Types.OutOfCredits ->
            Evergreen.V44.Translation.Types.OutOfCredits

        Evergreen.V43.Types.ApiProtocolError p0 ->
            Evergreen.V44.Translation.Types.ApiProtocolError (p0 |> migrate_Translation_Types_ProtocolError)

        Evergreen.V43.Types.GptDecodeError p0 ->
            Evergreen.V44.Translation.Types.GptDecodeError p0

        Evergreen.V43.Types.GptExpressedError p0 ->
            Evergreen.V44.Translation.Types.GptExpressedError p0


migrate_Translation_Types_ProtocolError : Evergreen.V43.Types.ProtocolError -> Evergreen.V44.Translation.Types.ProtocolError
migrate_Translation_Types_ProtocolError old =
    case old of
        Evergreen.V43.Types.RateLimited ->
            Evergreen.V44.Translation.Types.RateLimited

        Evergreen.V43.Types.HttpError p0 ->
            Evergreen.V44.Translation.Types.HttpError p0


migrate_Translation_Types_Translation : Evergreen.V43.Types.Translation -> Evergreen.V44.Translation.Types.Translation
migrate_Translation_Types_Translation old =
    { breakdown = old.breakdown
    , translatedText = old.translation
    , translatedTo = old.translatedTo |> migrate_Types_EnglishOrEstonian
    }


migrate_Types_EnglishOrEstonian : Evergreen.V43.Types.EnglishOrEstonian -> Evergreen.V44.Translation.Types.EnglishOrEstonian
migrate_Types_EnglishOrEstonian old =
    case old of
        Evergreen.V43.Types.English ->
            Evergreen.V44.Translation.Types.English

        Evergreen.V43.Types.Estonian ->
            Evergreen.V44.Translation.Types.Estonian


migrate_Types_AdminData : Evergreen.V43.Types.AdminData -> Evergreen.V44.Types.AdminData
migrate_Types_AdminData old =
    { emailsAndConsents = old.emailsAndConsents
    , adminMessages = []
    }


migrate_Types_BackendMsg : Evergreen.V43.Types.BackendMsg -> Evergreen.V44.Types.BackendMsg
migrate_Types_BackendMsg old =
    case old of
        Evergreen.V43.Types.NoOpBackendMsg ->
            Evergreen.V44.Types.NoOpBackendMsg

        Evergreen.V43.Types.AuthBackendMsg p0 ->
            Evergreen.V44.Types.AuthBackendMsg (p0 |> migrate_Auth_Common_BackendMsg)

        Evergreen.V43.Types.GptResponseReceived p0 p1 p2 p3 ->
            Evergreen.V44.Types.NoOpBackendMsg

        Evergreen.V43.Types.AddPublicCredits ->
            Evergreen.V44.Types.AddPublicCredits

        Evergreen.V43.Types.UpdateNow p0 ->
            Evergreen.V44.Types.NoOpBackendMsg


migrate_Types_FrontendMsg : Evergreen.V43.Types.FrontendMsg -> Evergreen.V44.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V43.Types.NoOpFrontendMsg ->
            Evergreen.V44.Types.NoOpFrontendMsg

        Evergreen.V43.Types.AuthSigninRequested p0 ->
            Evergreen.V44.Types.NoOpFrontendMsg

        Evergreen.V43.Types.UrlClicked p0 ->
            Evergreen.V44.Types.UrlClicked p0

        Evergreen.V43.Types.UrlChanged p0 ->
            Evergreen.V44.Types.UrlChanged p0

        Evergreen.V43.Types.GotViewport p0 ->
            Evergreen.V44.Types.GotViewport p0

        Evergreen.V43.Types.Resize p0 p1 ->
            Evergreen.V44.Types.Resize p0 p1

        Evergreen.V43.Types.TranslationInputModelChanged p0 ->
            Evergreen.V44.Types.NoOpFrontendMsg

        Evergreen.V43.Types.SubmitText p0 p1 ->
            Evergreen.V44.Types.SubmitText p0 p1

        Evergreen.V43.Types.ShowExplanation p0 ->
            Evergreen.V44.Types.NoOpFrontendMsg

        Evergreen.V43.Types.CycleLoadingAnimation ->
            Evergreen.V44.Types.CycleLoadingAnimation

        Evergreen.V43.Types.EditTranslation p0 ->
            Evergreen.V44.Types.EditTranslation p0

        Evergreen.V43.Types.GotoRoute p0 ->
            Evergreen.V44.Types.NoOpFrontendMsg

        Evergreen.V43.Types.GotoTranslate_FocusAndClear ->
            Evergreen.V44.Types.GotoTranslate_FocusAndClear

        Evergreen.V43.Types.StartSignup ->
            Evergreen.V44.Types.NoOpFrontendMsg

        Evergreen.V43.Types.SubmitSignupClicked p0 ->
            Evergreen.V44.Types.NoOpFrontendMsg

        Evergreen.V43.Types.SignupFormChanged p0 ->
            Evergreen.V44.Types.NoOpFrontendMsg

        Evergreen.V43.Types.FetchImportantNumber ->
            Evergreen.V44.Types.FetchImportantNumber

        Evergreen.V43.Types.Animate p0 ->
            Evergreen.V44.Types.Animate p0

        Evergreen.V43.Types.FiddleRandomBackroundPath p0 ->
            Evergreen.V44.Types.FiddleRandomBackroundPath p0

        Evergreen.V43.Types.ShowCreditCounterTooltip p0 ->
            Evergreen.V44.Types.ShowCreditCounterTooltip p0

        Evergreen.V43.Types.SetPublicConsentChecked p0 ->
            Evergreen.V44.Types.NoOpFrontendMsg


migrate_Types_FrontendUserInfo : Evergreen.V43.Auth.Common.UserInfo -> Evergreen.V44.Types.FrontendUserInfo
migrate_Types_FrontendUserInfo old =
    { id = 0
    , email = old.email
    , membershipStatus = Evergreen.V44.Types.NoStripeInfo
    , consentsSubmitted = False
    , publicConsentChecked = False
    }


migrate_Types_GeneralData : Evergreen.V43.Types.GeneralData -> Evergreen.V44.Types.GeneralData
migrate_Types_GeneralData old =
    { publicCreditsInfo =
        { current = old.publicCredits
        , nextRefresh = Time.millisToPosix 0
        , refreshAmount = 2
        }
    }


migrate_Types_SessionInfo : Evergreen.V43.Types.UserInfo -> Evergreen.V44.Types.SessionInfo
migrate_Types_SessionInfo old =
    { maybeAuthedUserId = Nothing
    , redirectReturnPage = Nothing
    }


migrate_Types_ToBackend : Evergreen.V43.Types.ToBackend -> Evergreen.V44.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V43.Types.NoOpToBackend ->
            Evergreen.V44.Types.NoOpToBackend

        Evergreen.V43.Types.AuthToBackend p0 ->
            Evergreen.V44.Types.AuthToBackend (p0 |> migrate_Auth_Common_ToBackend)

        Evergreen.V43.Types.SubmitTextForTranslation p0 p1 ->
            Evergreen.V44.Types.SubmitTextForTranslation p0 p1

        Evergreen.V43.Types.SubmitSignup p0 ->
            Evergreen.V44.Types.NoOpToBackend

        Evergreen.V43.Types.RequestImportantNumber ->
            Evergreen.V44.Types.NoOpToBackend

        Evergreen.V43.Types.RequestGeneralData ->
            Evergreen.V44.Types.RequestGeneralData


migrate_Types_ToFrontend : Evergreen.V43.Types.ToFrontend -> Evergreen.V44.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V43.Types.NoOpToFrontend ->
            Evergreen.V44.Types.NoOpToFrontend

        Evergreen.V43.Types.AuthToFrontend p0 ->
            Evergreen.V44.Types.AuthToFrontend (p0 |> migrate_Auth_Common_ToFrontend)

        Evergreen.V43.Types.AuthSuccess p0 ->
            Evergreen.V44.Types.AuthSuccess (p0 |> migrate_Types_FrontendUserInfo)

        Evergreen.V43.Types.TranslationResult p0 p1 ->
            Evergreen.V44.Types.NoOpToFrontend

        Evergreen.V43.Types.EmailSubmitAck ->
            Evergreen.V44.Types.NoOpToFrontend

        Evergreen.V43.Types.AdminDataMsg p0 ->
            Evergreen.V44.Types.AdminDataMsg (p0 |> migrate_Types_AdminData)

        Evergreen.V43.Types.GeneralDataMsg p0 ->
            Evergreen.V44.Types.GeneralDataMsg (p0 |> migrate_Types_GeneralData)

        Evergreen.V43.Types.CreditsUpdated p0 ->
            Evergreen.V44.Types.NoOpToFrontend
