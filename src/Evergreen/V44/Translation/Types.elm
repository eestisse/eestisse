module Evergreen.V44.Translation.Types exposing (..)

import Http
import Time


type EnglishOrEstonian
    = English
    | Estonian


type alias BreakdownPart =
    { estonian : String
    , englishTranslation : String
    , maybeExplanation : Maybe String
    }


type alias Breakdown =
    List BreakdownPart


type alias Translation =
    { translatedTo : EnglishOrEstonian
    , translatedText : String
    , breakdown : Breakdown
    }


type alias TranslationRecord =
    { id : Int
    , fromUserId : Maybe Int
    , public : Bool
    , time : Time.Posix
    , input : String
    , translation : Translation
    }


type ProtocolError
    = RateLimited
    | HttpError Http.Error


type GptAssistError
    = OutOfCredits
    | ApiProtocolError ProtocolError
    | GptDecodeError String
    | GptExpressedError String


type PublicOrPersonal
    = Public
    | Personal
