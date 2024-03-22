module BackgroundAnimation exposing (..)

import Element exposing (Element)
import Element.Background as Background
import Time
import Types exposing (..)


view : Time.Posix -> Element FrontendMsg
view t =
    let
        bgcolor =
            Element.rgb 1 1 0
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color bgcolor
        ]
        Element.none
