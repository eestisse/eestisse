module Evergreen.V46.Background.Types exposing (..)

import Evergreen.V46.Point
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


type alias PathSection =
    { piece : PathPiece
    , endPointRelative : Evergreen.V46.Point.Point
    , startPointRelative : Evergreen.V46.Point.Point
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


type alias PathAcrossAnimationState =
    { pathAcrossTarget : PathAcross
    , animationStart : Time.Posix
    }


type alias Model =
    { seed : Random.Seed
    , animationTime : Time.Posix
    , pathsAcross : List ( PathAcross, Maybe PathAcrossAnimationState )
    }
