port module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Lamdera
import Route exposing (Route)
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
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        route =
            Route.parseUrl url
    in
    ( { key = key
      , route = route
      , showExplainerSubtitle =
            case route of
                Route.Translate ->
                    True

                _ ->
                    False
      , translationPageModel =
            InputtingText ""

      -- RequestSent <| Loading "test stuff" 1
      -- RequestSent <| RequestComplete Testing.completedRequestExample
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
                    , Cmd.none
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

        HideExplainer ->
            ( { model | showExplainerSubtitle = False }
            , Cmd.none
            )

        GotoRoute route ->
            ( { model
                | route = route
                , showExplainerSubtitle = False
              }
            , Nav.pushUrl
                model.key
                (Route.routeToString route)
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


view : Model -> Browser.Document FrontendMsg
view model =
    View.root model


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    case model.translationPageModel of
        RequestSent (Waiting _ _) ->
            Time.every 1500 (always CycleLoadingAnimation)

        _ ->
            Sub.none


plausibleEventOutCmd : String -> Cmd msg
plausibleEventOutCmd name =
    plausible_event_out name


port plausible_event_out : String -> Cmd msg
