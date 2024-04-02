module Responsive exposing (..)


type DisplayProfile
    = Desktop
    | Mobile


screenWidthToDisplayProfile : Int -> DisplayProfile
screenWidthToDisplayProfile width =
    if width >= 1150 then
        Desktop

    else
        Mobile


responsiveVal : DisplayProfile -> a -> a -> a
responsiveVal dProfile mobileVal desktopVal =
    case dProfile of
        Mobile ->
            mobileVal

        Desktop ->
            desktopVal
