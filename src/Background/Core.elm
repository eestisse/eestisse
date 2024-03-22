module Background.Core exposing (..)

import Background.Config as Config
import List.Extra
import Random


type alias Model =
    { seed : Random.Seed
    , singlePathAcross : PathAcross
    }


type alias PathAcross =
    { elbows : List Elbow
    , newColor : ( Float, Float, Float )
    }


type alias Elbow =
    { yEnd : Float
    , xLength : Float
    }


init : Int -> Model
init seedInt =
    let
        ( singlePathAcross, seed ) =
            Random.step pathAcrossGenerator (Random.initialSeed seedInt)
    in
    { seed = seed
    , singlePathAcross = singlePathAcross
    }


elbowGenerator : Random.Generator Elbow
elbowGenerator =
    Random.map2
        Elbow
        (Random.float (Tuple.first Config.elbowXLengthRange) (Tuple.second Config.elbowXLengthRange))
        (Random.float (Tuple.first Config.elbowYRange) (Tuple.second Config.elbowYRange))


pathAcrossGenerator : Random.Generator PathAcross
pathAcrossGenerator =
    Random.map2
        PathAcross
        (Random.list Config.numElbowsInPath elbowGenerator
            |> Random.map correctElbowList
        )
        colorGenerator


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
                                        elbow.yEnd - prevElbow.yEnd
                                in
                                if yChange > 0 && yChange < Config.elbowRadius * 2 then
                                    { elbow | yEnd = prevElbow.yEnd + Config.elbowRadius * 2 }

                                else if yChange < 0 && yChange > negate Config.elbowRadius * 2 then
                                    { elbow | yEnd = prevElbow.yEnd - Config.elbowRadius * 2 }

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
