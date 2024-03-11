module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Lamdera
import Route exposing (Route)
import Testing
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
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , route = Route.parseUrl url
      , translationPageModel =
            { textInput = ""
            , requestState =
                NotSubmitted

            -- RequestComplete
            --     Testing.completedRequestExample
            }
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        TextInputChanged text ->
            ( { model
                | translationPageModel =
                    let
                        t =
                            model.translationPageModel
                    in
                    { t | textInput = text }
              }
            , Cmd.none
            )

        SubmitText inputText ->
            ( { model
                | translationPageModel =
                    let
                        t =
                            model.translationPageModel
                    in
                    { t
                        | requestState =
                            Loading inputText
                        , textInput = ""
                    }
              }
            , Lamdera.sendToBackend <| SubmitTextForTranslation inputText
            )

        ShowExplanation breakdownPart ->
            case model.translationPageModel.requestState of
                RequestComplete completedRequest ->
                    ( { model
                        | translationPageModel =
                            let
                                t =
                                    model.translationPageModel
                            in
                            { t
                                | requestState =
                                    RequestComplete
                                        { completedRequest
                                            | maybeSelectedBreakdownPart =
                                                Just breakdownPart
                                        }
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        TranslationResult inputText translationResult ->
            ( { model
                | translationPageModel =
                    let
                        t =
                            model.translationPageModel
                    in
                    { t
                        | requestState =
                            RequestComplete
                                { inputText = inputText
                                , translationResult = translationResult
                                , maybeSelectedBreakdownPart = Nothing
                                }
                    }
              }
            , Cmd.none
            )


view : Model -> Browser.Document FrontendMsg
view model =
    View.root model
