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
      , maybeTranslationResult = Nothing
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
            ( { model | textInput = text }
            , Cmd.none
            )

        SubmitText inputText ->
            ( model
            , Lamdera.sendToBackend <| SubmitTextForTranslation inputText
            )

        ShowExplanation phrase explanation ->
            case model.maybeTranslationResult of
                Just (Ok translation) ->
                    ( { model
                        | maybeTranslationResult =
                            Just <|
                                Ok <|
                                    { translation
                                        | selectedExplanation = Just ( phrase, explanation )
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

        TranslationResult translationResult ->
            ( { model | maybeTranslationResult = Just translationResult }
            , Cmd.none
            )


view : Model -> Browser.Document FrontendMsg
view model =
    View.root model
