port module Frontend exposing (..)

import Background.State as Background
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Lamdera
import Route exposing (Route)
import Task
import Testing
import Time
import Types exposing (..)
import Url
import View


type alias Model =
    FrontendModel


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


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        route =
            Route.parseUrl url
    in
    ( { key = key
      , route = route
      , translationPageModel =
            InputtingText ""

      -- RequestSent <| Loading "test stuff" 1
      -- RequestSent <| RequestComplete Testing.completedRequestExample
      , signupState = Inactive
      , maybeImportantNumber = Nothing
      , animationTime = Time.millisToPosix 0
      , backgroundModel = Nothing
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
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

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        TextInputChanged text ->
            ( { model
                | translationPageModel = InputtingText text
              }
            , Cmd.none
            )

        SubmitText inputText ->
            ( { model
                | translationPageModel =
                    RequestSent <| Waiting inputText 1
              }
            , Cmd.batch
                [ Lamdera.sendToBackend <| SubmitTextForTranslation inputText
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
                            if animationCounter == 4 then
                                1

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
                    InputtingText inputText
              }
            , Cmd.none
            )

        StartSignup ->
            ( { model | signupState = Active "" }
            , focusEmailInputCmd
            )

        SubmitSignup emailString ->
            ( { model | signupState = Submitting }
            , Lamdera.sendToBackend <| SubmitEmail emailString
            )

        SignupTextChanged text ->
            ( { model | signupState = Active text }
            , Cmd.none
            )

        GotoRoute route ->
            ( { model
                | route = route
              }
            , Nav.pushUrl
                model.key
                (Route.routeToString route)
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

                        Just _ ->
                            model.backgroundModel
              }
            , Cmd.none
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

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

        ImportantNumber number ->
            ( { model
                | maybeImportantNumber = Just number
              }
            , Cmd.none
            )


focusEmailInputCmd : Cmd FrontendMsg
focusEmailInputCmd =
    Task.attempt (\_ -> NoOpFrontendMsg) (Dom.focus "email-input")


plausibleEventOutCmd : String -> Cmd msg
plausibleEventOutCmd name =
    plausible_event_out name


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ case model.translationPageModel of
            RequestSent (Waiting _ _) ->
                Time.every 1500 (always CycleLoadingAnimation)

            _ ->
                Sub.none
        , case model.route of
            Route.Landing ->
                Browser.Events.onAnimationFrame Animate

            _ ->
                Sub.none
        ]


port plausible_event_out : String -> Cmd msg
