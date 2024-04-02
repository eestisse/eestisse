module Background.Types exposing (..)

import Background.Config as Config
import Random
import Time


type alias Model =
    { seed : Random.Seed
    , animationTime : Time.Posix
    , pathsAcross : List ( PathAcross, Maybe PathAcrossAnimationState )
    }


type alias Point =
    { x : Int
    , y : Int
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


addPoints : Point -> Point -> Point
addPoints a b =
    Point
        (a.x + b.x)
        (a.y + b.y)


interpolatePathsAcross : Float -> PathAcross -> PathAcross -> PathAcross
interpolatePathsAcross progressFloat oldPathAcross newPathAcross =
    -- assuming the piece types don't change
    { oldPathAcross
        | sections =
            List.map2 (interpolateSection progressFloat) oldPathAcross.sections newPathAcross.sections
    }


interpolateSection : Float -> PathSection -> PathSection -> PathSection
interpolateSection progressFloat oldSection newSection =
    { piece = interpolatePiece progressFloat oldSection.piece newSection.piece
    , startPointRelative = interpolatePoint progressFloat oldSection.startPointRelative newSection.startPointRelative
    , endPointRelative = interpolatePoint progressFloat oldSection.endPointRelative newSection.endPointRelative
    }


interpolatePiece : Float -> PathPiece -> PathPiece -> PathPiece
interpolatePiece progressFloat oldPiece newPiece =
    case ( oldPiece, newPiece ) of
        ( Right oldLength, Right newLength ) ->
            Right <| interpolateInt progressFloat oldLength newLength

        ( Up oldLength, Up newLength ) ->
            Up <| interpolateInt progressFloat oldLength newLength

        ( Down oldLength, Down newLength ) ->
            Down <| interpolateInt progressFloat oldLength newLength

        -- we assume the piece types don't change
        -- and we don't do anything to pieces without length
        ( otherOldPiece, _ ) ->
            otherOldPiece


interpolatePoint : Float -> Point -> Point -> Point
interpolatePoint progressFloat oldPoint newPoint =
    { x = interpolateInt progressFloat oldPoint.x newPoint.x
    , y = interpolateInt progressFloat oldPoint.y newPoint.y
    }


interpolateInt : Float -> Int -> Int -> Int
interpolateInt progressFloat old new =
    interpolateFloat progressFloat (toFloat old) (toFloat new)
        |> round


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat progressFloat old new =
    old + ((new - old) * progressFloat)


getRenderablePath : Time.Posix -> ( PathAcross, Maybe PathAcrossAnimationState ) -> PathAcross
getRenderablePath animationTime ( path, maybeAnimationState ) =
    let
        getProgressFloat startTime now =
            let
                timeDiffMillis =
                    Time.posixToMillis now - Time.posixToMillis startTime
            in
            min
                (toFloat timeDiffMillis / Config.pathAnimationTimeLengthMillis)
                1.0
                |> Config.easingFunction
    in
    case maybeAnimationState of
        Just animationState ->
            interpolatePathsAcross
                (getProgressFloat animationState.animationStart animationTime)
                path
                animationState.pathAcrossTarget

        Nothing ->
            path
