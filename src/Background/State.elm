module Background.State exposing (..)

import Background.Config as Config
import Background.Types exposing (..)
import Colors
import Element
import List.Extra
import Point exposing (Point)
import Random
import Random.Extra
import Time
import Types exposing (..)
import Utils


init : Time.Posix -> Model
init now =
    let
        seedInt =
            Time.posixToMillis now

        ( pathsAcross, seed ) =
            generatePathsAcross (Random.initialSeed seedInt)
    in
    { seed = seed
    , animationTime = now
    , pathsAcross = pathsAcross |> List.map (\p -> ( p, Nothing ))
    }


generatePathsAcross : Random.Seed -> ( List PathAcross, Random.Seed )
generatePathsAcross seed0 =
    let
        iterativelyBuildPathsAcross : ( List PathAcross, Random.Seed ) -> ( List PathAcross, Random.Seed )
        iterativelyBuildPathsAcross ( existingPathsAcross, seed ) =
            let
                ( heightFilledSoFar, maybeLastColor ) =
                    case List.Extra.last existingPathsAcross of
                        Nothing ->
                            ( 0, Nothing )

                        Just pathAcross ->
                            ( pathAcross.yPathStart + (Config.pathAcrossYVariance // 2)
                            , Just pathAcross.color
                            )
            in
            if List.length existingPathsAcross >= Config.numPaths then
                ( existingPathsAcross, seed )

            else
                let
                    newColor =
                        let
                            indexOfLastColor =
                                maybeLastColor
                                    |> Maybe.andThen (\lastColor -> Config.colors |> List.map Utils.elementColorToRgb |> List.Extra.findIndex ((==) lastColor))

                            indexOfThisColor =
                                indexOfLastColor
                                    |> Maybe.map ((+) 1)
                                    |> Maybe.map
                                        (\i ->
                                            if i >= List.length Config.colors then
                                                0

                                            else
                                                i
                                        )
                                    |> Maybe.withDefault 0
                        in
                        Config.colors
                            |> List.Extra.getAt indexOfThisColor
                            |> Maybe.withDefault Colors.black

                    ( newPathAcross, newSeed ) =
                        generatePathAcross heightFilledSoFar (Utils.elementColorToRgb newColor) seed
                in
                iterativelyBuildPathsAcross ( existingPathsAcross ++ [ newPathAcross ], newSeed )
    in
    iterativelyBuildPathsAcross ( [], seed0 )


generatePathAcross : Int -> RGB -> Random.Seed -> ( PathAcross, Random.Seed )
generatePathAcross yPathStart color seed0 =
    let
        iterativelyBuildSectionList : ( List PathSection, Random.Seed ) -> ( List PathSection, Random.Seed )
        iterativelyBuildSectionList ( existingPathSections, seed ) =
            let
                xStart =
                    List.Extra.last existingPathSections
                        |> Maybe.map .endPointRelative
                        |> Maybe.map .x
                        |> Maybe.withDefault 0
            in
            if xStart >= Config.minTotalWidth then
                ( existingPathSections, seed )

            else
                iterativelyBuildSectionList <| addNewSection ( existingPathSections, seed )

        ( sections, seed1 ) =
            iterativelyBuildSectionList ( [], seed0 )
    in
    ( { yPathStart = yPathStart
      , sections = sections
      , color = color
      }
    , seed1
    )


addNewSection : ( List PathSection, Random.Seed ) -> ( List PathSection, Random.Seed )
addNewSection ( existingSections, seed ) =
    let
        sectionGenerator : Random.Generator PathSection
        sectionGenerator =
            case List.Extra.last existingSections of
                Nothing ->
                    Random.uniform (always ElbowLeftToUp)
                        [ always ElbowLeftToDown
                        , always ElbowUpToRight
                        , always ElbowDownToRight
                        , Right
                        ]
                        |> Random.Extra.andMap (Random.int Config.horizontalSegmentXMin (Config.horizontalSegmentXMin + Config.horizontalSegmentXVariance))
                        |> Random.map (pieceToSection (Point 0 0))

                Just lastSection ->
                    case lastSection.piece of
                        ElbowLeftToUp ->
                            upwardVeritcalSectionGenerator lastSection.endPointRelative

                        ElbowLeftToDown ->
                            downwardVeritcalSectionGenerator lastSection.endPointRelative

                        ElbowUpToRight ->
                            rightSectionGenerator lastSection.endPointRelative

                        ElbowDownToRight ->
                            rightSectionGenerator lastSection.endPointRelative

                        Right _ ->
                            elbowFromLeftGenerator lastSection.endPointRelative

                        Up _ ->
                            Random.constant <|
                                pieceToSection lastSection.endPointRelative ElbowDownToRight

                        Down _ ->
                            Random.constant <|
                                pieceToSection lastSection.endPointRelative ElbowUpToRight
    in
    Random.step sectionGenerator seed
        |> Tuple.mapFirst
            (\newSection ->
                existingSections ++ [ newSection ]
            )


rightSectionGenerator : Point -> Random.Generator PathSection
rightSectionGenerator startPoint =
    let
        xMin =
            Config.horizontalSegmentXMin

        xMax =
            xMin + Config.horizontalSegmentXVariance
    in
    Random.map
        Right
        (Random.int xMin xMax)
        |> Random.map (pieceToSection startPoint)


maxYRelative : Int
maxYRelative =
    Config.pathAcrossYVariance // 2


minYRelative : Int
minYRelative =
    negate maxYRelative


upwardVeritcalSectionGenerator : Point -> Random.Generator PathSection
upwardVeritcalSectionGenerator startPoint =
    let
        verticalSpaceAvailable =
            startPoint.y - minYRelative - Config.elbowRadius

        maxValue =
            min verticalSpaceAvailable Config.maxVerticalLength
    in
    Random.map
        Up
        (Random.int 0 maxValue)
        |> Random.map (pieceToSection startPoint)


downwardVeritcalSectionGenerator : Point -> Random.Generator PathSection
downwardVeritcalSectionGenerator startPoint =
    let
        verticalSpaceAvailable =
            maxYRelative - startPoint.y - Config.elbowRadius

        maxValue =
            min verticalSpaceAvailable Config.maxVerticalLength
    in
    Random.map
        Down
        (Random.int 0 maxValue)
        |> Random.map (pieceToSection startPoint)


elbowFromLeftGenerator : Point -> Random.Generator PathSection
elbowFromLeftGenerator startPoint =
    Random.map (pieceToSection startPoint) <|
        if startPoint.y - Config.elbowRadius * 2 < minYRelative then
            Random.constant ElbowLeftToDown

        else if startPoint.y + Config.elbowRadius * 2 > maxYRelative then
            Random.constant ElbowLeftToUp

        else
            Random.Extra.choice ElbowLeftToDown ElbowLeftToUp


pieceTransformVector : PathPiece -> Point
pieceTransformVector piece =
    let
        downRight =
            { x = Config.elbowRadius
            , y = Config.elbowRadius
            }

        upRight =
            { x = Config.elbowRadius
            , y = negate Config.elbowRadius
            }
    in
    case piece of
        ElbowLeftToUp ->
            upRight

        ElbowLeftToDown ->
            downRight

        ElbowUpToRight ->
            downRight

        ElbowDownToRight ->
            upRight

        Right length ->
            { x = length
            , y = 0
            }

        Up length ->
            { x = 0
            , y = negate length
            }

        Down length ->
            { x = 0
            , y = length
            }


pieceToSection : Point -> PathPiece -> PathSection
pieceToSection startPoint piece =
    { piece = piece
    , endPointRelative = Point.add startPoint (pieceTransformVector piece)
    , startPointRelative = startPoint
    }


colorGenerator : Random.Generator RGB
colorGenerator =
    Random.uniform Colors.vibrantTeal
        [ Colors.coralPink
        , Colors.softOrange
        , Colors.sunshineYellow
        , Colors.lavender
        ]
        |> Random.map Utils.elementColorToRgb


tweakPathAcross : ( PathAcross, Random.Seed ) -> ( PathAcross, Random.Seed )
tweakPathAcross ( pathAcross, seed ) =
    let
        ( newSections, newSeed ) =
            getModifiedSectionsAcross ( pathAcross.sections, seed )
    in
    ( { pathAcross | sections = newSections }
    , newSeed
    )


getModifiedSectionsAcross : ( List PathSection, Random.Seed ) -> ( List PathSection, Random.Seed )
getModifiedSectionsAcross ( pathSections, seed0 ) =
    let
        ( ( _, finalSeed ), newSections ) =
            pathSections
                |> List.Extra.mapAccuml
                    (\( maybeNewSectionStartPoint, seed ) section ->
                        let
                            newStartPoint =
                                maybeNewSectionStartPoint |> Maybe.withDefault section.startPointRelative

                            ( newPiece, newSeed ) =
                                ( section.piece, seed ) |> tweakPiece
                        in
                        ( ( Just <| Point.add newStartPoint (pieceTransformVector newPiece), newSeed )
                        , pieceToSection newStartPoint newPiece
                        )
                    )
                    ( Nothing, seed0 )
    in
    ( newSections, finalSeed )


tweakPiece : ( PathPiece, Random.Seed ) -> ( PathPiece, Random.Seed )
tweakPiece ( pathPiece, seed0 ) =
    case pathPiece of
        Right _ ->
            Random.step
                (Random.int Config.horizontalSegmentXMin (Config.horizontalSegmentXMin + Config.horizontalSegmentXVariance))
                seed0
                |> Tuple.mapFirst Right

        Up _ ->
            Random.step
                (Random.int 0 Config.verticalSegmentTweakMaxLength)
                seed0
                |> Tuple.mapFirst Up

        Down _ ->
            Random.step
                (Random.int 0 Config.verticalSegmentTweakMaxLength)
                seed0
                |> Tuple.mapFirst Down

        _ ->
            ( pathPiece, seed0 )


clearFinishedAnimations : Model -> Model
clearFinishedAnimations model =
    { model
        | pathsAcross =
            model.pathsAcross
                |> List.map
                    (\( pathAcross, maybeAnimationState ) ->
                        case maybeAnimationState of
                            Nothing ->
                                ( pathAcross, Nothing )

                            Just animationState ->
                                let
                                    animationAgeInMillis =
                                        Time.posixToMillis model.animationTime - Time.posixToMillis animationState.animationStart
                                in
                                if animationAgeInMillis > Config.pathAnimationTimeLengthMillis then
                                    ( animationState.pathAcrossTarget, Nothing )

                                else
                                    ( pathAcross, Just animationState )
                    )
    }


fiddleRandomPath : Model -> Model
fiddleRandomPath model =
    let
        ( pathNum, seed1 ) =
            Random.step
                (Random.int 0 (Config.numPaths - 1))
                model.seed
    in
    case List.Extra.getAt pathNum model.pathsAcross of
        Nothing ->
            model

        Just ( pathAcross, maybeAnimationState ) ->
            let
                currentlyRenderedPathAcross =
                    getRenderablePath model.animationTime ( pathAcross, maybeAnimationState )

                ( newPathAcross, seed2 ) =
                    tweakPathAcross ( currentlyRenderedPathAcross, seed1 )
            in
            { model
                | pathsAcross =
                    model.pathsAcross
                        |> List.Extra.setAt pathNum
                            ( currentlyRenderedPathAcross
                            , Just <|
                                PathAcrossAnimationState
                                    newPathAcross
                                    model.animationTime
                            )
                , seed = seed2
            }


fiddleAllPaths : Model -> Model
fiddleAllPaths model =
    let
        ( newSeed, newPathsAcross ) =
            model.pathsAcross
                |> List.Extra.mapAccuml
                    (\seed ( pathAcross, maybeAnimationState ) ->
                        let
                            currentlyRenderedPathAcross =
                                getRenderablePath model.animationTime ( pathAcross, maybeAnimationState )
                        in
                        let
                            ( pathAcrossTarget, newSeed_ ) =
                                tweakPathAcross ( currentlyRenderedPathAcross, seed )
                        in
                        ( newSeed_
                        , ( currentlyRenderedPathAcross
                          , Just <|
                                { pathAcrossTarget = pathAcrossTarget
                                , animationStart = model.animationTime
                                }
                          )
                        )
                    )
                    model.seed
    in
    { model
        | pathsAcross = newPathsAcross
        , seed = newSeed
    }


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
    Utils.interpolateFloats progressFloat (toFloat old) (toFloat new)
        |> round


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
