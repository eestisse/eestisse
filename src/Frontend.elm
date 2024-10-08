port module Frontend exposing (..)

import Auth
import Auth.Common
import Auth.Flow
import Background.State as Background
import Background.Types as Background
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Config
import Dict exposing (Dict)
import Lamdera
import List.Extra
import Responsive exposing (..)
import Route exposing (Route)
import Task
import Testing
import Time
import Translation.Types exposing (..)
import Types exposing (..)
import Url
import Url.Builder
import Utils
import View


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = View.root
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        route =
            Route.parseUrl url

        ( model, routeCmd ) =
            { key = key
            , route = route
            , authFlow = Auth.Common.Idle
            , authRedirectBaseUrl = { url | query = Nothing, fragment = Nothing }
            , maybeAuthedUserInfo = Nothing
            , signinModel = { emailFormMode = Inactive }
            , dProfile = Nothing
            , maybeAdminData = Nothing
            , animationTime = Time.millisToPosix 0
            , time_bySecond = Time.millisToPosix 0
            , backgroundModel = Nothing
            , maybePublicCreditsInfo = Nothing
            , cachedTranslationRecords = Dict.empty
            , doTranslateModel =
                { input = ""
                , state = Inputting
                }
            , publicConsentChecked = True
            , viewTranslationModel = { maybeSelectedBreakdownPartId = Nothing }
            , loadingAnimationCounter = 0
            , mobileMenuOpen = False
            , noMorePublicTranslationsToFetch = False
            , noMorePersonalTranslationsToFetch = False
            , fetchingRecords = False
            , maybeConsentsFormModel = Nothing
            , feedbackFormModel = blankFeedbackFormModel
            }
                |> afterArrivingAtRoute route
    in
    (case route of
        Route.AuthCallback methodId ->
            Auth.Flow.init
                { model | route = Route.Account }
                methodId
                url
                key
                (\msg -> Lamdera.sendToBackend (TB_AuthMsg msg))

        _ ->
            ( model, Cmd.none )
    )
        |> Tuple.mapSecond
            (\maybeAuthCmd ->
                Cmd.batch
                    [ getViewportCmd
                    , if route == Route.Admin then
                        Lamdera.sendToBackend R_AdminData

                      else
                        Cmd.none
                    , Lamdera.sendToBackend R_GeneralData
                    , maybeAuthCmd
                    , routeCmd
                    , fakeConsoleLogCmd
                    ]
            )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        F_NoOp ->
            ( model, Cmd.none )

        StartGoogleSignin ->
            Auth.Flow.signInRequested "OAuthGoogle" model Nothing
                |> Tuple.mapSecond (TB_AuthMsg >> Lamdera.sendToBackend)
                |> Tuple.mapSecond
                    (\authCmd ->
                        Cmd.batch
                            [ Lamdera.sendToBackend <| TB_SetPostAuthRedirect model.route
                            , authCmd
                            ]
                    )

        StartEmailSignin ->
            ( { model | signinModel = { emailFormMode = InputtingEmail "" } }
            , Cmd.none
            )

        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( { model | route = Route.parseUrl url }
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            let
                route =
                    Route.parseUrl url
            in
            { model | route = route }
                |> afterArrivingAtRoute route

        GotViewport viewport ->
            ( { model
                | dProfile =
                    Just <| screenWidthToDisplayProfile <| floor viewport.viewport.width
              }
            , Cmd.none
            )

        Resize width _ ->
            ( { model
                | dProfile =
                    Just <| screenWidthToDisplayProfile width
              }
            , Cmd.none
            )

        ChangeTranslationInput s ->
            ( { model
                | doTranslateModel =
                    let
                        old =
                            model.doTranslateModel
                    in
                    { old
                        | input = s
                    }
              }
            , Cmd.none
            )

        ChangePublicConsentChecked flag ->
            ( { model
                | publicConsentChecked = flag
              }
            , case model.maybeAuthedUserInfo of
                Nothing ->
                    Cmd.none

                Just _ ->
                    Lamdera.sendToBackend <| TB_SetPublicTranslateChecked flag
            )

        SubmitTextForTranslation publicConsentChecked inputText ->
            ( { model
                | doTranslateModel =
                    { input = inputText
                    , state = TranslateRequestSubmitted
                    }
              }
            , Cmd.batch
                [ Lamdera.sendToBackend <| TB_TextForTranslation publicConsentChecked inputText
                , plausibleEventOutCmd "translation-requested"
                ]
            )

        ShowBreakdown breakdownPartId ->
            ( { model
                | viewTranslationModel =
                    let
                        oldVTM =
                            model.viewTranslationModel
                    in
                    { oldVTM | maybeSelectedBreakdownPartId = Just breakdownPartId }
              }
            , plausibleEventOutCmd "breakdown-shown"
            )

        CycleLoadingAnimation ->
            let
                newAnimationCounter =
                    if model.loadingAnimationCounter == 3 then
                        0

                    else
                        model.loadingAnimationCounter + 1
            in
            ( { model
                | loadingAnimationCounter = newAnimationCounter
              }
            , Cmd.none
            )

        GotoTranslateForm input ->
            { model
                | doTranslateModel =
                    { input = input
                    , state = Inputting
                    }
            }
                |> gotoRouteAndAnimate Route.Translate

        GotoRouteAndAnimate route ->
            gotoRouteAndAnimate route model

        GotoTranslate_FocusAndClear ->
            gotoRouteAndAnimate Route.Translate model
                |> Tuple.mapBoth
                    (\model_ ->
                        { model_
                            | doTranslateModel =
                                let
                                    old =
                                        model.doTranslateModel
                                in
                                { old
                                    | input = ""
                                }
                        }
                    )
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , focusTranslateInputCmd
                            ]
                    )

        Animate time ->
            ( { model
                | animationTime = time
                , backgroundModel =
                    case model.backgroundModel of
                        Nothing ->
                            Just <| Background.init time

                        Just bgModel ->
                            Just <|
                                Background.clearFinishedAnimations <|
                                    { bgModel | animationTime = time }
              }
            , Cmd.none
            )

        FiddleRandomBackroundPath _ ->
            case model.backgroundModel of
                Nothing ->
                    ( model, Cmd.none )

                Just backgroundModel ->
                    ( { model
                        | backgroundModel = Just <| Background.fiddleRandomPath backgroundModel
                      }
                    , Cmd.none
                    )

        StartStripePayment userId ->
            let
                targetLink =
                    Url.Builder.crossOrigin Config.stripePaymentLinkBaseUrl
                        [ Config.stripePaymentLinkId ]
                        [ Url.Builder.string "client_reference_id" (String.fromInt userId) ]
            in
            ( model
            , Nav.load targetLink
            )

        UserIntent_ActivateMembership ->
            gotoRouteAndAnimate Route.Account model

        Logout ->
            ( model
            , Lamdera.sendToBackend TB_Logout
            )

        UpdateFrontendNow_BySecond now ->
            ( { model | time_bySecond = now }
            , Cmd.none
            )

        ToggleMobileMenu ->
            ( { model | mobileMenuOpen = not model.mobileMenuOpen }
            , Cmd.none
            )

        FetchMoreTranslations publicOrPersonal countInfo ->
            ( { model | fetchingRecords = True }
            , Lamdera.sendToBackend <| R_TranslationRecords publicOrPersonal countInfo
            )

        ChangeEmailForm newForm ->
            ( { model
                | signinModel = { emailFormMode = newForm }
              }
            , Cmd.none
            )

        SubmitEmailForSignin email ->
            ( { model
                | signinModel = { emailFormMode = InputtingCode <| InputtingCodeModel email "" Nothing }
              }
            , Lamdera.sendToBackend <| R_EmailLoginCode email
            )

        SubmitEmailSigninCode email code ->
            ( { model
                | signinModel = { emailFormMode = CodeSubmitted email }
              }
            , Lamdera.sendToBackend <| TB_EmailSigninCode email code
            )

        ChangeConsentsForm newForm ->
            ( { model
                | maybeConsentsFormModel =
                    Just newForm
              }
            , Cmd.none
            )

        SubmitConsentsForm form ->
            ( model
            , Lamdera.sendToBackend <| TB_Consents form
            )

        ChangeFeedbackForm newForm ->
            ( { model | feedbackFormModel = newForm }
            , Cmd.none
            )

        SubmitFeedback isUser maybeEmail text ->
            ( { model
                | feedbackFormModel =
                    let
                        old =
                            model.feedbackFormModel
                    in
                    { old
                        | submitStatus = SubmitWaiting
                    }
              }
            , Lamdera.sendToBackend <| TB_UserFeedback isUser maybeEmail text
            )

        MarkAdminMessagesRead t ->
            ( model
            , Lamdera.sendToBackend <| TB_SetAdminMessagesLastRead t
            )

        TestAdminError ->
            ( model
            , Lamdera.sendToBackend <| TB_TestAdminError "wowow"
            )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TF_NoOp ->
            ( model, Cmd.none )

        TF_AuthMsg authToFrontendMsg ->
            Auth.updateFromBackend authToFrontendMsg model

        TF_AuthSuccess frontendUserInfo ->
            ( { model
                | maybeAuthedUserInfo = Just (Just frontendUserInfo)
                , publicConsentChecked = frontendUserInfo.publicConsentChecked
              }
            , Cmd.none
            )

        TF_TranslationResult inputText translationRecordResult ->
            let
                ( newCachedTranslationRecords, newDoTranslateModel ) =
                    case translationRecordResult of
                        Ok translationRecord ->
                            ( model.cachedTranslationRecords
                                |> Dict.insert
                                    translationRecord.id
                                    translationRecord
                            , { input = "", state = Inputting }
                            )

                        Err err ->
                            ( model.cachedTranslationRecords
                            , { input = model.doTranslateModel.input, state = Error err }
                            )

                newModel =
                    { model
                        | cachedTranslationRecords = newCachedTranslationRecords
                        , doTranslateModel = newDoTranslateModel
                    }
            in
            if model.route == Route.Translate && model.doTranslateModel.state == TranslateRequestSubmitted && inputText == model.doTranslateModel.input then
                case translationRecordResult of
                    Ok translationRecord ->
                        gotoRouteAndAnimate (Route.View translationRecord.id) newModel
                            |> Tuple.mapSecond
                                (\cmd ->
                                    Cmd.batch
                                        [ cmd
                                        , plausibleEventOutCmd "translation-complete"
                                        ]
                                )

                    Err _ ->
                        ( newModel
                        , plausibleEventOutCmd "translation-error"
                        )

            else
                -- ignore; the result has come back but the user has moved away from the page
                ( model, Cmd.none )

        TF_AdminData adminData ->
            ( { model
                | maybeAdminData = Just adminData
              }
            , Cmd.none
            )

        TF_GeneralData generalData ->
            ( { model | maybePublicCreditsInfo = Just generalData.publicCreditsInfo }
            , Cmd.none
            )

        TF_CreditsInfo newCreditsInfo ->
            ( { model | maybePublicCreditsInfo = Just newCreditsInfo }
            , Cmd.none
            )

        TF_TranslationRecordsRequestResult translationRecordsResult ->
            case translationRecordsResult of
                Err trFetchError ->
                    ( { model | fetchingRecords = False }
                    , consoleErr <| trFetchErrorToString trFetchError
                    )

                Ok translationRecords ->
                    ( { model
                        | fetchingRecords = False
                        , cachedTranslationRecords =
                            Dict.union
                                (translationRecords
                                    |> List.map (\tr -> ( tr.id, tr ))
                                    |> Dict.fromList
                                )
                                model.cachedTranslationRecords
                      }
                    , Cmd.none
                    )

        TF_RedirectReturnPage maybeReturnRoute ->
            case model.route of
                Route.AuthCallback _ ->
                    model
                        |> gotoRouteAndAnimate
                            (maybeReturnRoute
                                |> Maybe.withDefault Route.Landing
                            )

                _ ->
                    ( model, Cmd.none )

        TF_NoMoreTranslationsToFetch publicOrPersonal ->
            ( case publicOrPersonal of
                Public ->
                    { model | noMorePublicTranslationsToFetch = True }

                Personal ->
                    { model | noMorePersonalTranslationsToFetch = True }
            , Cmd.none
            )

        TF_UserInfo userInfo ->
            ( { model | maybeAuthedUserInfo = Just userInfo }
            , Cmd.none
            )

        TF_LoginCodeError err ->
            case model.signinModel.emailFormMode of
                CodeSubmitted emailAddress ->
                    ( { model
                        | signinModel = { emailFormMode = InputtingCode <| InputtingCodeModel emailAddress "" (Just err) }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TF_AckUserFeedback ->
            ( { model
                | feedbackFormModel =
                    { textInput = ""
                    , emailInput = ""
                    , submitStatus = Complete
                    }
              }
            , Cmd.none
            )


gotoRouteAndAnimate : Route -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
gotoRouteAndAnimate route model =
    ( { model
        | route = route
        , mobileMenuOpen = False
        , backgroundModel =
            model.backgroundModel
                |> Maybe.map Background.fiddleAllPaths
      }
    , Nav.pushUrl
        model.key
        (Route.routeToString route)
    )


focusEmailInputCmd : Cmd FrontendMsg
focusEmailInputCmd =
    Task.attempt (\_ -> F_NoOp) (Browser.Dom.focus "email-input")


focusTranslateInputCmd : Cmd FrontendMsg
focusTranslateInputCmd =
    Task.attempt (\_ -> F_NoOp) (Browser.Dom.focus "translate-input")


plausibleEventOutCmd : String -> Cmd msg
plausibleEventOutCmd name =
    plausible_event_out name


getViewportCmd : Cmd FrontendMsg
getViewportCmd =
    Browser.Dom.getViewport
        |> Task.perform GotViewport


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ case ( model.route, model.doTranslateModel.state ) of
            ( Route.Translate, TranslateRequestSubmitted ) ->
                Sub.batch
                    [ Time.every 900 (always CycleLoadingAnimation)
                    , Time.every 900 FiddleRandomBackroundPath
                    ]

            _ ->
                Sub.none
        , Browser.Events.onAnimationFrame Animate
        , Browser.Events.onResize Types.Resize
        , Time.every 1000 UpdateFrontendNow_BySecond
        ]


afterArrivingAtRoute : Route -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
afterArrivingAtRoute route model =
    case route of
        Route.AuthCallback _ ->
            ( model, Lamdera.sendToBackend <| R_AndClearRedirectReturnPage )

        Route.Browse ->
            ( model, Lamdera.sendToBackend <| R_TranslationRecords Public ( Nothing, Config.frontendFetchRecordCount ) )

        Route.History ->
            ( model, Lamdera.sendToBackend <| R_TranslationRecords Personal ( Nothing, Config.frontendFetchRecordCount ) )

        Route.View id ->
            case getTranslationRecord id model of
                Just _ ->
                    ( { model
                        | viewTranslationModel =
                            { maybeSelectedBreakdownPartId = Nothing }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | viewTranslationModel =
                            { maybeSelectedBreakdownPartId = Nothing }
                      }
                    , Lamdera.sendToBackend <| R_SingleTranslationRecord id
                    )

        _ ->
            ( model, Cmd.none )


port plausible_event_out : String -> Cmd msg


port consoleLog : String -> Cmd msg


fakeConsoleLogCmd =
    if False then
        consoleLog "just doing this to keep the port alive"

    else
        Cmd.none


port consoleErr : String -> Cmd msg
