module Background.View exposing (..)

import Background.Config as Config
import Background.Types exposing (..)
import Colors
import Element exposing (Element)
import Element.Background
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
                    [ shadowFilterSvg ]
                    :: List.map (renderPath dProfile time) model.pathsAcross
                )


shadowFilterSvg : Svg FrontendMsg
shadowFilterSvg =
    Svg.filter
        [ Svg.Attributes.id "shadow" ]
        [ Svg.node "feDropShadow"
            [ Svg.Attributes.dx "-5"
            , Svg.Attributes.dy "-5"
            , Svg.Attributes.stdDeviation "5"
            , Svg.Attributes.floodOpacity "0.5"
            ]
            []
        ]


useShadowSvgAttribute : Svg.Attribute FrontendMsg
useShadowSvgAttribute =
    Svg.Attributes.style "filter:url(#shadow)"


renderPath : DisplayProfile -> Time.Posix -> ( PathAcross, Maybe PathAcrossAnimationState ) -> Svg FrontendMsg
renderPath dProfile animationTime ( path, maybeAnimationState ) =
    let
        getProgressFloat startTime now =
            let
                timeDiffMillis =
                    Time.posixToMillis now - Time.posixToMillis startTime
            in
            min
                (toFloat timeDiffMillis / Config.pathAnimationTimeLengthMillis)
                1

        pathToRender =
            case maybeAnimationState of
                Just animationState ->
                    interpolatePathsAcross
                        (getProgressFloat animationState.animationStart animationTime)
                        path
                        animationState.pathAcrossTarget

                Nothing ->
                    path

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
        , Svg.Attributes.fill <| rgbToSvgString pathToRender.color
        , Svg.Attributes.stroke <| colorToSvgString Config.pathColor
        , Svg.Attributes.strokeWidth <| String.fromInt Config.pathThickness
        , useShadowSvgAttribute
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
        |> floor


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat progressFloat old new =
    old + ((new - old) * progressFloat)
