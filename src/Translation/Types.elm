module Translation.Types exposing (..)

import Http
import Time


type EnglishOrEstonian
    = English
    | Estonian


type alias Breakdown =
    List BreakdownPart


type alias BreakdownPart =
    { estonian : String
    , englishTranslation : String
    , maybeExplanation : Maybe String
    }


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


type GptAssistError
    = OutOfCredits
    | TooLong Int
    | ApiProtocolError ProtocolError
    | GptDecodeError String
    | GptExpressedError String


type ProtocolError
    = RateLimited
    | HttpError Http.Error


type PublicOrPersonal
    = Public
    | Personal
