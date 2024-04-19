module Auth exposing (..)

import Auth.Common
import Auth.Flow
import Auth.Method.EmailMagicLink
import Auth.Method.OAuthGithub
import Auth.Method.OAuthGoogle
import Dict exposing (Dict)
import Dict.Extra
import Env
import Lamdera
import Time
import Time.Extra
import Types exposing (..)
import Utils


config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend FrontendModel BackendModel
config =
    { toBackend = AuthToBackend
    , toFrontend = AuthToFrontend
    , backendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , sendToBackend = Lamdera.sendToBackend
    , methods =
        [ Auth.Method.OAuthGoogle.configuration Env.googleAppClientId Env.googleAppClientSecret
        ]
    , renewSession = renewSession
    }


backendConfig : BackendModel -> Auth.Flow.BackendUpdateConfig FrontendMsg BackendMsg ToFrontend FrontendModel BackendModel
backendConfig model =
    { asToFrontend = AuthToFrontend
    , asBackendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , backendModel = model
    , loadMethod = Auth.Flow.methodLoader config.methods
    , handleAuthSuccess = handleAuthSuccess model
    , isDev = Env.mode == Env.Development
    , renewSession = renewSession
    , logout = logout
    }


renewSession : Lamdera.SessionId -> Lamdera.ClientId -> BackendModel -> ( BackendModel, Cmd BackendMsg )
renewSession _ _ model =
    ( model, Cmd.none )


handleAuthSuccess :
    BackendModel
    -> Lamdera.SessionId
    -> Lamdera.ClientId
    -> Auth.Common.UserInfo
    -> Auth.Common.MethodId
    -> Maybe Auth.Common.Token
    -> Time.Posix
    -> ( BackendModel, Cmd BackendMsg )
handleAuthSuccess backendModel sessionId clientId authUserInfo _ _ _ =
    -- TODO handle renewing sessions if that is something you need
    let
        ( newModel, ( userId, userInfo ) ) =
            addOrGetUserFromEmail authUserInfo.email backendModel

        sessionsWithoutThisOne : Dict Lamdera.SessionId Int
        sessionsWithoutThisOne =
            Dict.Extra.removeWhen (\_ id -> id == userId) backendModel.authedSessions

        newSessions =
            Dict.insert sessionId userId sessionsWithoutThisOne

        response =
            AuthSuccess <| toFrontendUserInfo ( userId, userInfo, userMembershipStatus backendModel.nowish userInfo )
    in
    ( { newModel | authedSessions = newSessions }
    , Cmd.batch
        [ -- renewSession_ user_.id sessionId clientId
          Lamdera.sendToFrontend clientId response
        ]
    )


addOrGetUserFromEmail : String -> BackendModel -> ( BackendModel, ( Int, UserInfo ) )
addOrGetUserFromEmail email model =
    let
        maybeFoundUser =
            model.users
                |> Dict.Extra.find
                    (\_ userInfo ->
                        userInfo.email == email
                    )
    in
    case maybeFoundUser of
        Just foundUserAndId ->
            ( model, foundUserAndId )

        Nothing ->
            let
                newUser : UserInfo
                newUser =
                    { email = email
                    , stripeInfo = Nothing
                    }
            in
            ( { model
                | users =
                    model.users
                        |> Dict.insert model.nextUserId newUser
                , nextUserId = model.nextUserId + 1
              }
            , ( model.nextUserId, newUser )
            )


logout : Lamdera.SessionId -> Lamdera.ClientId -> BackendModel -> ( BackendModel, Cmd msg )
logout sessionId _ model =
    ( { model | authedSessions = model.authedSessions |> Dict.remove sessionId }, Cmd.none )


updateFromBackend authToFrontendMsg model =
    case authToFrontendMsg of
        Auth.Common.AuthInitiateSignin url ->
            Auth.Flow.startProviderSignin url model

        Auth.Common.AuthError err ->
            Auth.Flow.setError model err

        Auth.Common.AuthSessionChallenge _ ->
            ( model, Cmd.none )


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
