module CommonTypes exposing (..)


type DisplayProfile
    = Desktop
    | Mobile


screenWidthToDisplayProfile : Int -> DisplayProfile
screenWidthToDisplayProfile width =
    if width >= 1150 then
        Desktop

    else
        Mobile
