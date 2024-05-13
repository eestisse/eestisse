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

        model : FrontendModel
        model =
            { key = key
            , route = route
            , authFlow = Auth.Common.Idle
            , authRedirectBaseUrl = { url | query = Nothing, fragment = Nothing }
            , maybeAuthedUserInfo = Nothing
            , signinModel = { emailFormMode = Inactive }
            , dProfile = Nothing
            , maybeAdminData = Nothing
            , animationTime = Time.millisToPosix 0
            , time_updatePerSecond = Time.millisToPosix 0
            , backgroundModel = Nothing
            , maybePublicCreditsInfo = Nothing
            , showCreditCounterTooltip = False
            , creditsCounterAnimationState = Nothing
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
            }

        routeCmd =
            arriveAtRouteCmds route model
    in
    (case route of
        Route.AuthCallback methodId ->
            Auth.Flow.init
                { model | route = Route.Subscribe }
                methodId
                url
                key
                (\msg -> Lamdera.sendToBackend (AuthToBackend msg))

        _ ->
            ( model, Cmd.none )
    )
        |> Tuple.mapSecond
            (\maybeAuthCmd ->
                Cmd.batch
                    [ getViewportCmd
                    , if route == Route.Admin then
                        Lamdera.sendToBackend RequestImportantNumber

                      else
                        Cmd.none
                    , Lamdera.sendToBackend RequestGeneralData
                    , maybeAuthCmd
                    , routeCmd
                    ]
            )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GoogleSigninRequested ->
            Auth.Flow.signInRequested "OAuthGoogle" model Nothing
                |> Tuple.mapSecond (AuthToBackend >> Lamdera.sendToBackend)
                |> Tuple.mapSecond
                    (\authCmd ->
                        Cmd.batch
                            [ Lamdera.sendToBackend <| SetPostAuthRedirect model.route
                            , authCmd
                            ]
                    )

        EmailSigninRequested ->
            ( model, Cmd.none )

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
            ( { model | route = route }
            , arriveAtRouteCmds route model
            )

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

        TranslationInputChanged s ->
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

        PublicConsentChecked flag ->
            ( { model
                | publicConsentChecked = flag
              }
            , Cmd.none
            )

        SubmitText publicConsentChecked inputText ->
            ( { model
                | doTranslateModel =
                    { input = inputText
                    , state = TranslateRequestSubmitted
                    }
              }
            , Cmd.batch
                [ Lamdera.sendToBackend <| SubmitTextForTranslation publicConsentChecked inputText
                , plausibleEventOutCmd "translation-requested"
                ]
            )

        ShowExplanation breakdownPartId ->
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

        EditTranslation input ->
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

        FetchImportantNumber ->
            ( model
            , Lamdera.sendToBackend RequestImportantNumber
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

        ShowCreditCounterTooltip flag ->
            ( { model | showCreditCounterTooltip = flag }
            , Cmd.none
            )

        TriggerStripePayment userId ->
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
            gotoRouteAndAnimate Route.Subscribe model

        Logout ->
            ( model
            , Lamdera.sendToBackend DoLogout
            )

        UpdateFrontendNow now ->
            ( { model | time_updatePerSecond = now }
            , Cmd.none
            )

        ToggleMobileMenu ->
            ( { model | mobileMenuOpen = not model.mobileMenuOpen }
            , Cmd.none
            )

        LoadMoreClicked publicOrPersonal countInfo ->
            ( { model | fetchingRecords = True }
            , Lamdera.sendToBackend <| RequestTranslations publicOrPersonal countInfo
            )

        ChangeEmailForm newForm ->
            ( { model
                | signinModel = { emailFormMode = newForm }
              }
            , Cmd.none
            )

        SubmitEmailClicked email ->
            ( { model
                | signinModel = { emailFormMode = InputtingCode email "" }
              }
            , Lamdera.sendToBackend <| RequestEmailLoginCode email
            )

        SubmitCodeClicked email code ->
            ( { model
                | signinModel = { emailFormMode = CodeSubmitted }
              }
            , Lamdera.sendToBackend <| SubmitCodeForEmail email code
            )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        AuthToFrontend authToFrontendMsg ->
            Auth.updateFromBackend authToFrontendMsg model

        AuthSuccess frontendUserInfo ->
            ( { model | maybeAuthedUserInfo = Just frontendUserInfo }
            , Cmd.none
            )

        TranslationResult inputText translationRecordResult ->
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

        AdminDataMsg adminData ->
            ( { model
                | maybeAdminData = Just adminData
              }
            , Cmd.none
            )

        GeneralDataMsg generalData ->
            ( { model | maybePublicCreditsInfo = Just generalData.publicCreditsInfo }
            , Cmd.none
            )

        CreditsInfoUpdated newCreditsInfo ->
            ( { model | maybePublicCreditsInfo = Just newCreditsInfo }
                |> startCreditCounterAnimation (newCreditsInfo.current >= (model.maybePublicCreditsInfo |> Maybe.map .current |> Maybe.withDefault 0)) model.animationTime
            , Cmd.none
            )

        RequestTranslationRecordsResult translationRecordsResult ->
            case translationRecordsResult of
                Err errStr ->
                    let
                        _ =
                            Debug.log "error fetching translationRecord:" errStr
                    in
                    ( { model | fetchingRecords = False }, Cmd.none )

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

        RequestRedirectReturnPageResult maybeReturnRoute ->
            case model.route of
                Route.AuthCallback _ ->
                    model
                        |> gotoRouteAndAnimate
                            (maybeReturnRoute
                                |> Maybe.withDefault Route.Landing
                            )

                _ ->
                    ( model, Cmd.none )

        LogoutAck ->
            ( { model
                | maybeAuthedUserInfo = Nothing
              }
            , Cmd.none
            )

        NoMoreTranslationsToFetch publicOrPersonal ->
            ( case publicOrPersonal of
                Public ->
                    { model | noMorePublicTranslationsToFetch = True }

                Personal ->
                    { model | noMorePersonalTranslationsToFetch = True }
            , Cmd.none
            )


startCreditCounterAnimation : Bool -> Time.Posix -> FrontendModel -> FrontendModel
startCreditCounterAnimation goingUp now model =
    { model
        | creditsCounterAnimationState =
            Just <|
                { goingUp = goingUp
                , startTime = now
                }
    }


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
    Task.attempt (\_ -> NoOpFrontendMsg) (Browser.Dom.focus "email-input")


focusTranslateInputCmd : Cmd FrontendMsg
focusTranslateInputCmd =
    Task.attempt (\_ -> NoOpFrontendMsg) (Browser.Dom.focus "translate-input")


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
        , Time.every 1000 UpdateFrontendNow
        ]


arriveAtRouteCmds : Route -> FrontendModel -> Cmd FrontendMsg
arriveAtRouteCmds route model =
    case route of
        Route.AuthCallback _ ->
            Lamdera.sendToBackend <| RequestAndClearRedirectReturnPage

        Route.Browse ->
            Lamdera.sendToBackend <| RequestTranslations Public ( Nothing, Config.frontendFetchRecordCount )

        Route.History ->
            Lamdera.sendToBackend <| RequestTranslations Personal ( Nothing, Config.frontendFetchRecordCount )

        Route.View id ->
            case getTranslationRecord id model of
                Just _ ->
                    Cmd.none

                Nothing ->
                    Lamdera.sendToBackend <| RequestTranslation id

        _ ->
            Cmd.none


port plausible_event_out : String -> Cmd msg
