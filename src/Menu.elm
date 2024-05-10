module Menu exposing (..)

import Colors
import CommonView exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Responsive exposing (..)
import Route exposing (Route)
import Types exposing (..)


viewMenu : DisplayProfile -> Route -> Element FrontendMsg
viewMenu dProfile currentRoute =
    Element.column
        [ Element.height Element.fill
        , Element.spacing 15
        , Element.paddingXY 0 10
        , Element.Background.color <| Element.rgb 0.9 0.9 1
        , Border.widthEach
            { right = 1
            , top = 1
            , bottom = 1
            , left = 0
            }
        , Border.color <| Element.rgb 0.7 0.7 0.7
        , Border.roundEach
            { topLeft = 0
            , bottomLeft = 0
            , topRight = 10
            , bottomRight = 10
            }
        , Border.shadow
            { offset = ( 3, 0 )
            , size = 0
            , blur = 15
            , color = Element.rgba 0 0 0 0.3
            }
        , Font.size <| responsiveVal dProfile 18 24
        ]
        [ if dProfile == Mobile then
            Input.button
                [ Element.alignRight
                , Element.paddingXY 5 0
                ]
                { onPress = Just ToggleMobileMenu
                , label =
                    Element.image
                        [ Element.height <| Element.px 30
                        ]
                        { src = "x.png"
                        , description = "close menu"
                        }
                }

          else
            Element.none
        , Element.column
            []
            (menuOptions
                |> List.map
                    (\option ->
                        viewOption dProfile (currentRoute == option.route) option
                    )
            )
        ]


type alias MenuOption =
    { label : String
    , route : Route
    }


menuOptions : List MenuOption
menuOptions =
    [ MenuOption "Account" Route.Account
    , MenuOption "Translate" Route.Translate
    , MenuOption "Your Translations" Route.BrowsePersonal
    , MenuOption "Public Translations" Route.Browse
    ]


viewOption : DisplayProfile -> Bool -> MenuOption -> Element FrontendMsg
viewOption dProfile onThisRoute option =
    let
        selectedAttributes =
            [ Element.Background.color <| Element.rgba 0 0 0 0.1 ]
    in
    Input.button
        ([ Element.padding 10
         , Element.width Element.fill
         ]
            ++ (if onThisRoute then
                    selectedAttributes

                else
                    []
               )
        )
        { onPress = Just <| GotoRouteAndAnimate option.route
        , label =
            Element.el
                [ Element.paddingXY 10 0
                , Font.color <| Element.rgb 0.3 0.3 0.2
                ]
            <|
                Element.text option.label
        }
