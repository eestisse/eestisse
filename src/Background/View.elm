module Background.View exposing (..)

import Background.Config as Config
import Background.Types exposing (..)
import Colors
import Element exposing (Element)
import Element.Background
import Svg exposing (Svg)
import Svg.Attributes
import Time
import Types exposing (..)
import Utils


view : Time.Posix -> Model -> Element FrontendMsg
view time model =
    let
        millisElapsed =
            Time.posixToMillis time - Time.posixToMillis model.startTime
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color <| Config.color1
        ]
    <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.height <| String.fromInt Config.minDrawableHeightToFill ]
                (List.map (renderPath millisElapsed) model.pathsAcross)


renderPath : Int -> PathAcross -> Svg FrontendMsg
renderPath millisElapsed path =
    let
        -- xOffset =
        --     if path.color == Utils.elementColorToRgb Config.color1 then
        --         -50 - (toFloat millisElapsed / 30.0)
        --     else if path.color == Utils.elementColorToRgb Config.color2 then
        --         -50 + (toFloat millisElapsed / 30.0)
        --     else
        --         -50
        xOffset =
            -50

        basePoint =
            { x = round xOffset, y = path.yPathStart }

        pathStartString =
            "M " ++ pointToString basePoint

        pathBodyString =
            path.sections
                |> List.map (makePathStringSnippet basePoint)
                |> String.join " "

        pathEndString =
            [ "L " ++ (pointToString <| addPoints basePoint { x = Config.minTotalWidth, y = Config.drawableShapeBottomY })
            , "L " ++ (pointToString <| addPoints basePoint { x = 0, y = Config.drawableShapeBottomY })
            , "Z"
            ]
                |> String.join " "

        dString =
            String.join " "
                [ pathStartString, pathBodyString, pathEndString ]
    in
    Svg.path
        [ Svg.Attributes.d dString
        , Svg.Attributes.fill <| colorToSvgString path.color
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeWidth "20"
        ]
        []


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


colorToSvgString : RGB -> String
colorToSvgString rgb =
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


boolToInt : Bool -> Int
boolToInt flag =
    if flag then
        1

    else
        0
