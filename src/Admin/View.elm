module Admin.View exposing (..)

import Colors
import CommonView exposing (..)
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


page : DisplayProfile -> Maybe AdminData -> Element FrontendMsg
page dProfile maybeNumbers =
    CommonView.primaryBox
        [ Element.centerX
        , Element.centerY
        , CommonView.madimiFont
        ]
    <|
        case maybeNumbers of
            Nothing ->
                Element.el [ Font.size 30 ] <| Element.text "The SUSPENSE....."

            Just adminData ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 40
                    , Font.size 40
                    ]
                    [ Element.row
                        [ Element.spacing 5
                        ]
                        [ Element.text "Number of paying users:"
                        , Element.text <| String.fromInt adminData.numPaidUsers
                        ]
                    , adminData.emailsAndConsents
                        |> List.map
                            (\( label, number ) ->
                                Element.row
                                    [ Element.width Element.fill
                                    , Element.spacing 40
                                    , Element.padding 3
                                    ]
                                    [ Element.el
                                        [ Element.alignRight
                                        , Font.color <| Colors.blue
                                        ]
                                      <|
                                        Element.text <|
                                            (String.fromInt number ++ " ")
                                    , Element.paragraph
                                        [ Font.size 20 ]
                                        [ Element.text label ]
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
