module Background.Types exposing (..)

import Point exposing (Point)
import Random
import Time


type alias Model =
    { seed : Random.Seed
    , animationTime : Time.Posix
    , pathsAcross : List ( PathAcross, Maybe PathAcrossAnimationState )
    }


type alias PathAcross =
    { yPathStart : Int
    , sections : List PathSection
    , color : { red : Float, green : Float, blue : Float }
    }


type alias PathAcrossAnimationState =
    { pathAcrossTarget : PathAcross
    , animationStart : Time.Posix
    }


type alias PathSection =
    { piece : PathPiece
    , endPointRelative : Point
    , startPointRelative : Point
    }


type PathPiece
    = ElbowLeftToUp
    | ElbowLeftToDown
    | ElbowUpToRight
    | ElbowDownToRight
    | Right Int
    | Up Int
    | Down Int
