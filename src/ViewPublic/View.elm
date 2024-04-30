module ViewPublic.View exposing (..)

import Element exposing (Element)
import Responsive exposing (..)
import Types exposing (..)
import ViewPublic.Types exposing (..)


page : DisplayProfile -> ViewPublicModel -> Element FrontendMsg
page dProfile model =
    Element.text "hi there from viewpublic"
