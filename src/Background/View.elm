module Background.View exposing (..)

import Background.Config as Config
import Background.Types exposing (..)
import Element exposing (Element)
import Svg
import Svg.Attributes
import Time
import Types exposing (..)


view : Time.Posix -> Model -> Element FrontendMsg
view time model =
    let
        basePoint =
            { x = 0, y = model.singlePathAcross.yPathStart }

        pathStartString =
            "M " ++ pointToString basePoint

        pathBodyString =
            model.singlePathAcross.sections
                |> List.map (makePathStringSnippet basePoint)
                |> String.join " "

        pathEndString =
            ""

        dString =
            String.join " "
                [ pathStartString, pathBodyString, pathEndString ]
    in
    Element.el [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.height "2000" ]
                [ Svg.path
                    [ Svg.Attributes.d dString
                    , Svg.Attributes.fill "red"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "20"
                    ]
                    []
                ]


makePathStringSnippet : Point -> PathSection -> String
makePathStringSnippet basePoint section =
    let
        realEndPoint =
            addPoints basePoint section.endPointRelative

        lineStr =
            "L " ++ pointToString realEndPoint

        makeArcStr : Bool -> Bool -> String
        makeArcStr largeArc clockwise =
            [ "A"
            , String.fromInt Config.elbowRadius -- rx
            , String.fromInt Config.elbowRadius -- ry
            , String.fromInt 0 -- rotation
            , String.fromInt <| boolToInt largeArc
            , String.fromInt <| boolToInt clockwise
            , String.fromInt realEndPoint.x
            , String.fromInt realEndPoint.y
            ]
                |> String.join " "
    in
    case section.piece of
        ElbowLeftToUp ->
            makeArcStr False False

        ElbowLeftToDown ->
            makeArcStr False True

        ElbowUpToRight ->
            makeArcStr False False

        ElbowDownToRight ->
            makeArcStr False True

        Right _ ->
            lineStr

        Up _ ->
            lineStr

        Down _ ->
            lineStr


pointToString : Point -> String
pointToString p =
    String.fromInt p.x ++ "," ++ String.fromInt p.y


boolToInt : Bool -> Int
boolToInt flag =
    if flag then
        1

    else
        0
