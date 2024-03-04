module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Http
import Lamdera
import Url


type GptAssistError
    = ApiProtocolError Http.Error
    | GptDecodeError String
    | GptExpressedError String


type alias Translation =
    { inputAndExplanations : List ( String, String )
    , translation : String
    , selectedExplanation : Maybe ( String, String )
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , textInput : String
    , maybeTranslationResult : Maybe (Result GptAssistError Translation)
    }


type alias BackendModel =
    { thing : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TextInputChanged String
    | SubmitText String
    | ShowExplanation String String


type ToBackend
    = NoOpToBackend
    | SubmitTextForTranslation String


type BackendMsg
    = NoOpBackendMsg
    | GptResponseReceived Lamdera.ClientId (Result Http.Error String)


type ToFrontend
    = NoOpToFrontend
    | TranslationResult (Result GptAssistError Translation)
