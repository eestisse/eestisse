module Background.View exposing (..)

import Background.Config as Config
import Background.State as State
import Background.Types exposing (..)
import Colors
import Element exposing (Element)
import Element.Background
import Point exposing (Point)
import Responsive exposing (..)
import Svg exposing (Svg)
import Svg.Attributes
import Time
import Types exposing (..)
import Utils


view : DisplayProfile -> Time.Posix -> Model -> Element FrontendMsg
view dProfile time model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color Config.firstSectionColor
        ]
    <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.height "100%" ]
                (Svg.defs
                    []
                    []
                    :: List.map (renderPath dProfile time) model.pathsAcross
                )


renderPath : DisplayProfile -> Time.Posix -> ( PathAcross, Maybe PathAcrossAnimationState ) -> Svg FrontendMsg
renderPath dProfile animationTime ( path, maybeAnimationState ) =
    let
        pathToRender =
            State.getRenderablePath animationTime ( path, maybeAnimationState )

        xOffset =
            -50

        basePoint =
            { x = round xOffset, y = pathToRender.yPathStart + Config.horizontalSpaceBeforePaths dProfile }

        pathStartString =
            "M " ++ pointToString basePoint

        pathBodyString =
            pathToRender.sections
                |> List.map (makePathStringSnippet basePoint)
                |> String.join " "

        pathEndString =
            [ "L " ++ (pointToString <| Point.add basePoint { x = Config.minTotalWidth, y = Config.drawableShapeBottomY })
            , "L " ++ (pointToString <| Point.add basePoint { x = 0, y = Config.drawableShapeBottomY })
            , "Z"
            ]
                |> String.join " "

        dString =
            String.join " "
                [ pathStartString, pathBodyString, pathEndString ]
    in
    Svg.path
        [ Svg.Attributes.d dString
        , Svg.Attributes.fill <| rgbToSvgString pathToRender.color
        , Svg.Attributes.stroke <| colorToSvgString Config.pathColor
        , Svg.Attributes.strokeWidth <| String.fromInt Config.pathThickness
        ]
        []


makePathStringSnippet : Point -> PathSection -> String
makePathStringSnippet basePoint section =
    let
        realEndPoint =
            Point.add basePoint section.endPointRelative

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


rgbToSvgString : RGB -> String
rgbToSvgString rgb =
    let
        numbersString =
            [ rgb.red
            , rgb.green
            , rgb.blue
            ]
                |> List.map
                    (\f ->
                        f * 255 |> floor
                    )
                |> List.map String.fromInt
                |> String.join " "
    in
    "rgb(" ++ numbersString ++ ")"


colorToSvgString : Element.Color -> String
colorToSvgString =
    Utils.elementColorToRgb >> rgbToSvgString


boolToInt : Bool -> Int
boolToInt flag =
    if flag then
        1

    else
        0
