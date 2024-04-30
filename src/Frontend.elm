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
import ViewPublic.State as ViewPublic


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
            , translationPageModel =
                InputtingText <|
                    TranslationInputModel
                        ""
                        True

            -- RequestSent <| Waiting "test stuff" 1
            -- RequestSent <| RequestComplete Testing.completedRequestExample
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
            , viewPublicModel = ViewPublic.init
            }
    in
    (case route of
        Route.AuthCallback methodId ->
            Auth.Flow.init
                model
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
            ( { model | route = Route.parseUrl url }
            , Cmd.none
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

        TranslationInputModelChanged newModel ->
            ( { model
                | translationPageModel = InputtingText newModel
              }
            , Cmd.none
            )

        SubmitText publicConsentChecked inputText ->
            ( { model
                | translationPageModel =
                    RequestSent <| Waiting inputText 0
              }
            , Cmd.batch
                [ Lamdera.sendToBackend <| SubmitTextForTranslation publicConsentChecked inputText
                , plausibleEventOutCmd "translation-requested"
                ]
            )

        ShowExplanation breakdownPart ->
            case model.translationPageModel of
                RequestSent (RequestComplete completedRequest) ->
                    ( { model
                        | translationPageModel =
                            RequestSent <|
                                RequestComplete
                                    { completedRequest
                                        | maybeSelectedBreakdownPart =
                                            Just breakdownPart
                                    }
                      }
                    , plausibleEventOutCmd "breakdown-shown"
                    )

                _ ->
                    ( model, Cmd.none )

        CycleLoadingAnimation ->
            case model.translationPageModel of
                RequestSent (Waiting text animationCounter) ->
                    let
                        newAnimationCounter =
                            if animationCounter == 3 then
                                0

                            else
                                animationCounter + 1
                    in
                    ( { model
                        | translationPageModel =
                            RequestSent <| Waiting text newAnimationCounter
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditTranslation inputText ->
            ( { model
                | translationPageModel =
                    InputtingText { input = inputText, publicConsentChecked = True }
              }
            , Cmd.none
            )

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
            gotoRouteAndAnimate model route

        GotoTranslate_FocusAndClear ->
            gotoRouteAndAnimate model Route.Translate
                |> Tuple.mapBoth
                    (\model_ ->
                        { model_
                            | translationPageModel = InputtingText { input = "", publicConsentChecked = True }
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
            gotoRouteAndAnimate model Route.Account

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

        TranslationResult inputText translationResult ->
            case model.translationPageModel of
                RequestSent (Waiting lastRequestedText _) ->
                    if inputText == lastRequestedText then
                        ( { model
                            | translationPageModel =
                                RequestSent <|
                                    RequestComplete
                                        { inputText = inputText
                                        , translationResult = translationResult
                                        , maybeSelectedBreakdownPart = Nothing
                                        }
                          }
                        , case translationResult of
                            Ok _ ->
                                plausibleEventOutCmd "translation-complete"

                            Err _ ->
                                plausibleEventOutCmd "translation-error"
                        )

                    else
                        -- ignore; we're getting a result from an older request
                        ( model, Cmd.none )

                _ ->
                    -- ignore, we're getting a result but the user has navigated away
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


startCreditCounterAnimation : Bool -> Time.Posix -> FrontendModel -> FrontendModel
startCreditCounterAnimation goingUp now model =
    { model
        | creditsCounterAnimationState =
            Just <|
                { goingUp = goingUp
                , startTime = now
                }
    }


gotoRouteAndAnimate : FrontendModel -> Route -> ( FrontendModel, Cmd FrontendMsg )
gotoRouteAndAnimate model route =
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
        [ case model.translationPageModel of
            RequestSent (Waiting _ _) ->
                Sub.batch
                    [ Time.every 900 (always CycleLoadingAnimation)
                    , Time.every 900 FiddleRandomBackroundPath
                    ]

            _ ->
                Sub.none
        , Browser.Events.onAnimationFrame Animate
        , Browser.Events.onResize Types.Resize
        ]


port plausible_event_out : String -> Cmd msg
