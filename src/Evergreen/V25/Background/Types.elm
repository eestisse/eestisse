module Evergreen.V25.Background.Types exposing (..)

import Random
import Time


type PathPiece
    = ElbowLeftToUp
    | ElbowLeftToDown
    | ElbowUpToRight
    | ElbowDownToRight
    | Right Int
    | Up Int
    | Down Int


type alias Point =
    { x : Int
    , y : Int
    }


type alias PathSection =
    { piece : PathPiece
    , endPointRelative : Point
    , startPointRelative : Point
    }


type alias PathAcross =
    { yPathStart : Int
    , sections : List PathSection
    , color :
        { red : Float
        , green : Float
        , blue : Float
        }
    }


type alias Model =
    { seed : Random.Seed
    , startTime : Time.Posix
    , pathsAcross : List PathAcross
    }
