module Background.Types exposing (..)

import Random


type alias Model =
    { seed : Random.Seed
    , pathsAcross : List PathAcross
    }


type alias Point =
    { x : Int
    , y : Int
    }


type alias PathAcross =
    { yPathStart : Int
    , sections : List PathSection
    , color : { red : Float, green : Float, blue : Float, alpha : Float }
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


addPoints : Point -> Point -> Point
addPoints a b =
    Point
        (a.x + b.x)
        (a.y + b.y)
