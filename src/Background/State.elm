module Background.State exposing (..)

import Background.Config as Config
import Background.Types exposing (..)
import Colors
import Element
import List.Extra
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
    , startTime = now
    , pathsAcross = pathsAcross
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
                            ( Config.horizontalSpaceBeforePaths, Nothing )

                        Just pathAcross ->
                            ( pathAcross.yPathStart + (Config.pathAcrossYVariance // 2)
                            , Just pathAcross.color
                            )
            in
            if heightFilledSoFar > Config.minDrawableHeightToFill then
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
generatePathAcross yMin color seed0 =
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

        ( yPathStart, seed1 ) =
            let
                yMax =
                    yMin
            in
            Random.step
                (Random.int yMin yMax)
                seed0

        ( sections, seed2 ) =
            iterativelyBuildSectionList ( [], seed1 )
    in
    ( { yPathStart = yPathStart
      , sections = sections
      , color = color
      }
    , seed2
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
    , endPointRelative = addPoints startPoint (pieceTransformVector piece)
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
