module Background.Core exposing (..)

import Background.Config as Config
import Colors
import List.Extra
import Random


type alias Model =
    { seed : Random.Seed
    , singlePathAcross : PathAcross
    }


type alias PathAcross =
    { elbows : List Elbow
    , color : ( Float, Float, Float )
    }


type alias Elbow =
    { yStart : Int
    , xEnd : Int
    }


init : Int -> Model
init seedInt =
    let
        ( singlePathAcross, seed ) =
            generatePathAcross 300 (Random.initialSeed seedInt)
    in
    { seed = seed
    , singlePathAcross = singlePathAcross
    }


generatePathAcross : Int -> Random.Seed -> ( PathAcross, Random.Seed )
generatePathAcross yMin seed0 =
    let
        yGenerator =
            Random.int yMin (yMin + Config.pathAcrossYVariance)

        genElbow xMin seed =
            Random.step
                (Random.map2
                    Elbow
                    yGenerator
                    (Random.int xMin (xMin + Config.horizontalSegmentXVariance))
                )
                seed

        buildElbowList : ( List Elbow, Random.Seed ) -> ( List Elbow, Random.Seed )
        buildElbowList ( existingList, seed ) =
            let
                xMin =
                    case List.Extra.last existingList of
                        Just lastElbow ->
                            lastElbow.xEnd + Config.horizontalSegmentXMin

                        Nothing ->
                            Config.horizontalSegmentXMin
            in
            if xMin > Config.width then
                ( existingList, seed )

            else
                buildElbowList
                    (let
                        ( elbow, newSeed ) =
                            genElbow xMin seed
                     in
                     ( existingList ++ [ elbow ], newSeed )
                    )

        ( elbowList, finalSeed ) =
            buildElbowList ( [], seed0 )
    in
    ( PathAcross
        (elbowList |> correctElbowList)
        ( 0, 0, 1 )
    , finalSeed
    )


correctElbowList : List Elbow -> List Elbow
correctElbowList elbows =
    let
        singlePassTweak =
            elbows
                |> List.indexedMap
                    (\i elbow ->
                        case List.Extra.getAt (i - 1) elbows of
                            Nothing ->
                                elbow

                            Just prevElbow ->
                                -- if they're too close together in Y, push them apart
                                let
                                    yChange =
                                        elbow.yStart - prevElbow.yStart
                                in
                                if yChange > 0 && yChange < Config.elbowRadius * 2 then
                                    { elbow | yStart = prevElbow.yStart + Config.elbowRadius * 2 }

                                else if yChange < 0 && yChange > negate Config.elbowRadius * 2 then
                                    { elbow | yStart = prevElbow.yStart - Config.elbowRadius * 2 }

                                else
                                    elbow
                    )
    in
    if elbows == singlePassTweak then
        singlePassTweak

    else
        correctElbowList singlePassTweak


colorGenerator : Random.Generator ( Float, Float, Float )
colorGenerator =
    Random.map3
        (\a b c -> ( a, b, c ))
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)
