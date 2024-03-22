module BackgroundAnimation exposing (..)

import Element exposing (Element)
import Time
import Types exposing (..)


view : Time.Posix -> Element FrontendMsg
view t =
    Element.text <| String.fromInt <| Time.posixToMillis t
