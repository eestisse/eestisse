module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Lamdera
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
      , textInput = ""
      , requestState =
            NotSubmitted

      -- RequestComplete
      --     testCompletedRequest
      }
    , Cmd.none
    )



-- testCompletedRequest =
--     { inputText = ""
--     , translationResult =
--         Ok <|
--             { breakdown =
--                 [ { estonian = "minu"
--                   , englishTranslation = "my"
--                   , maybeExplanation = Nothing
--                   }
--                 , { estonian = "nimi"
--                   , englishTranslation = "is"
--                   , maybeExplanation = Just "some explanation here some explanation here some explanation here some explanation here some explanation here some explanation here "
--                   }
--                 ]
--             , translation = "my name is Logan"
--             }
--     , maybeSelectedBreakdownPart = Nothing
--     }


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
            ( { model | textInput = text }
            , Cmd.none
            )

        SubmitText inputText ->
            ( { model
                | requestState =
                    Loading inputText
                , textInput = ""
              }
            , Lamdera.sendToBackend <| SubmitTextForTranslation inputText
            )

        ShowExplanation breakdownPart ->
            case model.requestState of
                RequestComplete completedRequest ->
                    ( { model
                        | requestState =
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


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        TranslationResult inputText translationResult ->
            ( { model
                | requestState =
                    RequestComplete
                        { inputText = inputText
                        , translationResult = translationResult
                        , maybeSelectedBreakdownPart = Nothing
                        }
              }
            , Cmd.none
            )


view : Model -> Browser.Document FrontendMsg
view model =
    View.root model
