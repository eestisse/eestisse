module Background.Config exposing (..)

import Colors
import Element
import Responsive exposing (..)


horizontalSpaceBeforePaths dProfile =
    responsiveVal dProfile 280 340


horizontalSegmentXMin =
    100


horizontalSegmentXVariance =
    200


verticalSegmentTweakMaxLength =
    40


minTotalWidth =
    4000


pathAcrossYVariance =
    300


elbowRadius =
    -- this must be less than 1/4 of pathAcrossYVariance
    30


drawableShapeBottomY =
    800


numPaths =
    6


maxVerticalLength =
    30


colors =
    [ Colors.vibrantTeal
    , Colors.sunshineYellow
    , Element.rgb 0.8 0.8 1

    -- , Element.rgb 0.5 1 0.5
    ]


firstSectionColor =
    Colors.calmTeal


pathColor =
    Colors.offWhite


pathThickness =
    10


pathAnimationTimeLengthMillis =
    1000


easingFunction : Float -> Float
easingFunction x =
    if x == 1 then
        1

    else
        1 - (2 ^ (-20 * x))
