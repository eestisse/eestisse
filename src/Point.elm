module Point exposing (..)


type alias Point =
    { x : Int
    , y : Int
    }


add : Point -> Point -> Point
add a b =
    Point
        (a.x + b.x)
        (a.y + b.y)
