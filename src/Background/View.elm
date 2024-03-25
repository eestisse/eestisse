module Background.View exposing (..)

import Background.Config as Config
import Background.Core as Core exposing (Point)
import Element exposing (Element)
import Svg
import Svg.Attributes
import Time
import Types exposing (..)


view : Time.Posix -> Core.Model -> Element FrontendMsg
view time model =
    let
        basePoint =
            { x = 0, y = model.singlePathAcross.yPathStart }

        pathStartString =
            "M " ++ pointToString basePoint

        pathBodyString =
            model.singlePathAcross.sections
                |> List.map
                    (\section ->
                        "L " ++ pointToString (Core.addPoints basePoint section.endPointRelative)
                    )
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
                [ Svg.Attributes.width (String.fromInt Config.width), Svg.Attributes.height "2000" ]
                [ Svg.path
                    [ Svg.Attributes.d dString
                    , Svg.Attributes.fill "red"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "20"
                    ]
                    []
                ]



-- yStart: 610 localhost:8000:262:10
-- section: (0,{ endPointRelative = { x = 40, y = -40 }, piece = ElbowLeftToUp }) localhost:8000:262:10
-- section: (1,{ endPointRelative = { x = 40, y = -102 }, piece = Up 62 }) localhost:8000:262:10
-- section: (2,{ endPointRelative = { x = 80, y = -142 }, piece = ElbowDownToRight }) localhost:8000:262:10
-- section: (3,{ endPointRelative = { x = 241, y = -142 }, piece = Right 161 }) localhost:8000:262:10
-- section: (4,{ endPointRelative = { x = 281, y = -102 }, piece = ElbowLeftToDown }) localhost:8000:262:10
-- section: (5,{ endPointRelative = { x = 281, y = -82 }, piece = Down 20 }) localhost:8000:262:10
-- section: (6,{ endPointRelative = { x = 321, y = -42 }, piece = ElbowUpToRight }) localhost:8000:262:10
-- section: (7,{ endPointRelative = { x = 428, y = -42 }, piece = Right 107 }) localhost:8000:262:10
-- section: (8,{ endPointRelative = { x = 468, y = -82 }, piece = ElbowLeftToUp }) localhost:8000:262:10
-- section: (9,{ endPointRelative = { x = 468, y = -118 }, piece = Up 36 }) localhost:8000:262:10
-- section: (10,{ endPointRelative = { x = 508, y = -158 }, piece = ElbowDownToRight }) localhost:8000:262:10
-- section: (11,{ endPointRelative = { x = 643, y = -158 }, piece = Right 135 }) localhost:8000:262:10
-- section: (12,{ endPointRelative = { x = 683, y = -118 }, piece = ElbowLeftToDown }) localhost:8000:262:10
-- section: (13,{ endPointRelative = { x = 683, y = -32 }, piece = Down 86 }) localhost:8000:262:10
-- section: (14,{ endPointRelative = { x = 723, y = 8 }, piece = ElbowUpToRight }) localhost:8000:262:10
-- section: (15,{ endPointRelative = { x = 838, y = 8 }, piece = Right 115 }) localhost:8000:262:10
-- section: (16,{ endPointRelative = { x = 878, y = 48 }, piece = ElbowLeftToDown }) localhost:8000:262:10
-- section: (17,{ endPointRelative = { x = 878, y = 79 }, piece = Down 31 }) localhost:8000:262:10
-- section: (18,{ endPointRelative = { x = 918, y = 119 }, piece = ElbowUpToRight }) localhost:8000:262:10
-- section: (19,{ endPointRelative = { x = 1104, y = 119 }, piece = Right 186 }) localhost:8000:262:10
-- section: (20,{ endPointRelative = { x = 1144, y = 159 }, piece = ElbowLeftToDown }) localhost:8000:262:10
-- section: (21,{ endPointRelative = { x = 1144, y = 150 }, piece = Down -9 }) localhost:8000:262:10
-- section: (22,{ endPointRelative = { x = 1184, y = 190 }, piece = ElbowUpToRight }) localhost:8000:262:10
-- section: (23,{ endPointRelative = { x = 1360, y = 190 }, piece = Right 176 }) localhost:8000:262:10
-- section: (24,{ endPointRelative = { x = 1400, y = 150 }, piece = ElbowLeftToUp }) localhost:8000:262:10
-- section: (25,{ endPointRelative = { x = 1400, y = 82 }, piece = Up 68 }) localhost:8000:262:10
-- section: (26,{ endPointRelative = { x = 1440, y = 42 }, piece = ElbowDownToRight }) localhost:8000:262:10
-- section: (27,{ endPointRelative = { x = 1695, y = 42 }, piece = Right 255 }) localhost:8000:262:10
-- section: (28,{ endPointRelative = { x = 1735, y = 2 }, piece = ElbowLeftToUp }) localhost:8000:262:10
-- section: (29,{ endPointRelative = { x = 1735, y = -59 }, piece = Up 61 }) localhost:8000:262:10
-- section: (30,{ endPointRelative = { x = 1775, y = -99 }, piece = ElbowDownToRight }) localhost:8000:262:10
-- section: (31,{ endPointRelative = { x = 1993, y = -99 }, piece = Right 218 }) localhost:8000:262:10
-- section: (32,{ endPointRelative = { x = 2033, y = -59 }, piece = ElbowLeftToDown })


pointToString : Point -> String
pointToString p =
    String.fromInt p.x ++ "," ++ String.fromInt p.y
