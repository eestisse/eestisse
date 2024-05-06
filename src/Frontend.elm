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
            , dProfile = Nothing
            , signupState = Inactive
            , maybeAdminData = Nothing
            , animationTime = Time.millisToPosix 0
            , backgroundModel = Nothing
            , publicCredits = Nothing
            , showCreditCounterTooltip = False
            , creditsCounterAnimationState = Nothing
            , authFlow = Auth.Common.Idle
            , authRedirectBaseUrl = { url | query = Nothing, fragment = Nothing }
            , maybeAuthedUserInfo = Nothing
            , cachedTranslationRecords = Dict.empty
            , doTranslateModel =
                { input = ""
                , state = Inputting
                }
            , publicConsentChecked = True
            , viewTranslationModel = { maybeSelectedBreakdownPartId = Nothing }
            , loadingAnimationCounter = 0
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

        AuthSigninRequested { methodId, username } ->
            Auth.Flow.signInRequested methodId model username
                |> Tuple.mapSecond (AuthToBackend >> Lamdera.sendToBackend)

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

        StartSignup ->
            ( { model | signupState = Active blankSignupForm }
            , focusEmailInputCmd
            )

        SubmitSignupClicked signupForm ->
            ( { model | signupState = Submitting }
            , Lamdera.sendToBackend <| SubmitSignup signupForm
            )

        SignupFormChanged newForm ->
            ( { model
                | signupState =
                    Active <|
                        newForm
              }
            , Cmd.none
            )

        GotoRoute route ->
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
            ( model, Nav.load targetLink )

        UserIntent_ActivateMembership ->
            gotoRouteAndAnimate Route.Subscribe model

        Logout ->
            ( model
            , Lamdera.sendToBackend DoLogout
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
                newCachedTranslationRecords =
                    case translationRecordResult of
                        Ok translationRecord ->
                            model.cachedTranslationRecords
                                |> Dict.insert
                                    translationRecord.id
                                    translationRecord

                        Err _ ->
                            model.cachedTranslationRecords

                newModel =
                    { model
                        | cachedTranslationRecords = newCachedTranslationRecords
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

                    Err gptAssistError ->
                        ( { newModel
                            | doTranslateModel =
                                let
                                    old =
                                        newModel.doTranslateModel
                                in
                                { old
                                    | state = Error gptAssistError
                                }
                          }
                        , plausibleEventOutCmd "translation-error"
                        )

                    else
                -- ignore; the result has come back but the user has moved away from the page
                    ( model, Cmd.none )

        EmailSubmitAck ->
            ( { model
                | signupState = Submitted
              }
            , plausibleEventOutCmd "email-signup"
            )

        AdminDataMsg adminData ->
            ( { model
                | maybeAdminData = Just adminData
              }
            , Cmd.none
            )

        GeneralDataMsg generalData ->
            ( { model | publicCredits = Just generalData.publicCredits }
            , Cmd.none
            )

        CreditsUpdated newCredits ->
            ( { model | publicCredits = Just newCredits }
                |> startCreditCounterAnimation (newCredits >= (model.publicCredits |> Maybe.withDefault 0)) model.animationTime
            , Cmd.none
            )

        RequestTranslationRecordsResult translationRecordsResult ->
            case translationRecordsResult of
                Err errStr ->
                    let
                        _ =
                            Debug.log "error fetching translationRecord:" errStr
                    in
                    ( model, Cmd.none )

                Ok translationRecords ->
                    ( { model
                        | cachedTranslationRecords =
                            Dict.union
                                (translationRecords
                                    |> List.map (\tr -> ( tr.id, tr ))
                                    |> Dict.fromList
                                )
                                model.cachedTranslationRecords
              }
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
        ]


arriveAtRouteCmds : Route -> FrontendModel -> Cmd FrontendMsg
arriveAtRouteCmds route model =
    case route of
        Route.Browse ->
            Lamdera.sendToBackend <| RequestPublicTranslations

        Route.View id ->
            case getTranslationRecord id model of
                Just _ ->
                    Cmd.none

                Nothing ->
                    Lamdera.sendToBackend <| RequestTranslation id

        _ ->
            Cmd.none


port plausible_event_out : String -> Cmd msg
