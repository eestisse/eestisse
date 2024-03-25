module Background.OldView exposing (..)

import Background.Config as Config
import Background.Core exposing (..)
import Element exposing (Element)
import Element.Background as Background
import List.Extra
import Svg
import Svg.Attributes
import Time
import Types exposing (..)


view : Time.Posix -> Background.Core.Model -> Element FrontendMsg
view time model =
    let
        elbowPoints =
            model.singlePathAcross.elbows
                |> List.indexedMap
                    (\i elbow ->
                        let
                            xStart =
                                case List.Extra.getAt (i - 1) model.singlePathAcross.elbows of
                                    Just prevElbow ->
                                        prevElbow.xEnd

                                    Nothing ->
                                        0

                            p1 =
                                ( xStart, elbow.yStart )

                            p2 =
                                ( elbow.xEnd, elbow.yStart )
                        in
                        [ p1, p2 ]
                    )
                |> List.concat

        pathStringMainPart =
            elbowPoints
                |> List.indexedMap
                    (\i point ->
                        (if i == 0 then
                            "M "

                         else
                            "L "
                        )
                            ++ pointToString point
                    )
                |> String.join " "

        pathStringFinalPart =
            let
                ( lastX, lastY ) =
                    elbowPoints
                        |> List.Extra.last
                        |> Maybe.withDefault ( 0, 0 )

                panelBottomY =
                    lastY + Config.heightOfPanelsFromLastY
            in
            [ "L " ++ pointToString ( lastX, panelBottomY )
            , "L " ++ pointToString ( 0, panelBottomY )
            , "Z"
            ]
                |> String.join " "

        dString =
            String.join " " [ pathStringMainPart, pathStringFinalPart ]
    in
    Element.el [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.width (String.fromInt Config.width), Svg.Attributes.height "2000" ]
                [ Svg.path
                    [ Svg.Attributes.d dString
                    , Svg.Attributes.fill "red"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "20"
                    ]
                    []
                ]


pointToString : ( Int, Int ) -> String
pointToString ( x, y ) =
    String.fromInt x ++ "," ++ String.fromInt y
