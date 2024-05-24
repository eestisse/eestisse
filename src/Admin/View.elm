module Admin.View exposing (..)

import Backend exposing (countPaidUsers)
import Colors
import CommonView exposing (..)
import Dict
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Responsive exposing (..)
import Time
import Time.Format
import Time.Format.Config.Config_en_us exposing (config)
import Types exposing (..)


page : DisplayProfile -> Time.Posix -> Maybe AdminData -> Element FrontendMsg
page dProfile now maybeAdminData =
    CommonView.primaryBox
        [ Element.centerX
        , Element.centerY
        ]
    <|
        case maybeAdminData of
            Nothing ->
                Element.el [ Font.size 30 ] <| Element.text "The SUSPENSE....."

            Just adminData ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 40
                    ]
                    [ Element.column
                        [ Element.width Element.fill
                        , Element.spacing 5
                        ]
                        [ Element.row
                            [ Element.spacing 5
                            ]
                            [ Element.text "Number of users:"
                            , Element.text <| String.fromInt <| List.length <| Dict.toList adminData.users
                            ]
                        , Element.row
                            [ Element.spacing 5
                            ]
                            [ Element.text "Number of paying users:"
                            , Element.text <| String.fromInt <| countPaidUsers adminData.users
                            ]
                        ]
                    , blueButton dProfile [] [] "Test admin error" <| Just TestAdminError
                    , adminData.users
                        |> Dict.toList
                        |> List.map
                            (\( id, userInfo ) ->
                                Element.row
                                    ([ Element.spacing 10
                                     , Border.width 1
                                     , Border.color Colors.blue
                                     ]
                                        ++ (if maybeBackendUserInfoMembershipActive (Just userInfo) now then
                                                [ Font.bold ]

                                            else
                                                []
                                           )
                                    )
                                    [ Element.text userInfo.email
                                    ]
                            )
                        |> Element.column
                            [ Element.spacing 40
                            ]
                    , Element.column
                        [ Element.spacing 10
                        ]
                        [ case maybeTimeOfMostRecentMessage adminData.adminMessages of
                            Just timeOfMostRecentMessage ->
                                blueButton dProfile [] [] "mark as read" <| Just <| MarkAdminMessagesRead <| timeOfMostRecentMessage

                            Nothing ->
                                Element.none
                        , Element.column
                            [ Element.spacing 5
                            , Font.size 18
                            ]
                            (adminData.adminMessages
                                |> List.map
                                    (\( time, message ) ->
                                        Element.row
                                            [ Element.spacing 10
                                            , Border.width 1
                                            , Border.color Colors.gray
                                            , Element.height (Element.shrink |> Element.maximum 400)
                                            ]
                                            [ Element.el [ Element.width <| Element.px 200, Element.alignTop ] <|
                                                Element.text <|
                                                    Time.Format.format config "%Y.%m.%d %H:%M:%S" Time.utc time
                                            , Element.paragraph [ Element.height Element.fill, Element.scrollbarY ] [ Element.text message ]
                                            ]
                                    )
                            )
                        ]
                    ]


maybeTimeOfMostRecentMessage : List ( Time.Posix, String ) -> Maybe Time.Posix
maybeTimeOfMostRecentMessage messages =
    messages
        |> List.sortBy (Tuple.first >> Time.posixToMillis)
        |> List.reverse
        |> List.head
        |> Maybe.map Tuple.first
