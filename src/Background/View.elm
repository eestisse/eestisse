module Background.View exposing (..)

import Background.Core exposing (..)
import Element exposing (Element)
import Element.Background as Background
import Svg
import Svg.Attributes
import Time
import Types exposing (..)


view : Time.Posix -> Background.Core.Model -> Element FrontendMsg
view time model =
    Element.el [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.width "2000", Svg.Attributes.height "2000", Svg.Attributes.viewBox "0 0 120 120" ]
                [ Svg.path
                    [ Svg.Attributes.d "M 50,0 L 0,100 L 100,100 Z"
                    , Svg.Attributes.fill "red"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "2"
                    ]
                    []
                ]
