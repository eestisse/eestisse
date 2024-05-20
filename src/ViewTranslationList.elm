module ViewTranslationList exposing (..)

import Colors
import CommonView exposing (..)
import Config
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Responsive exposing (..)
import Route
import Translation.Types exposing (..)
import Types exposing (..)


type FetchButtonVisibility
    = DontShow
    | Show
    | Loading


viewTranslationList : DisplayProfile -> Dict Int TranslationRecord -> PublicOrPersonal -> FetchButtonVisibility -> Element FrontendMsg
viewTranslationList dProfile translationRecords publicOrPersonal fetchButtonVisibility =
    scrollbarYEl
        [ Element.padding 5
        , Border.width 1
        , Border.color <| Element.rgba 0 0 0.5 0.4
        , Border.rounded 4
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.spacing <| responsiveVal dProfile 15 20
            ]
            [ Element.column
                [ Element.spacing <| responsiveVal dProfile 15 20
                , Element.width Element.fill
                ]
                (translationRecords
                    |> Dict.values
                    |> List.sortBy (.id >> negate)
                    |> List.map (viewTranslationRecordPreviewButton dProfile)
                )
            , case fetchButtonVisibility of
                DontShow ->
                    Element.none

                Show ->
                    Element.el [ Element.centerX ] <| loadMoreButton dProfile publicOrPersonal (getLowestId translationRecords)

                Loading ->
                    Element.el [ Element.centerX ] <| loadingSnake [ Element.height <| Element.px 50 ]
            ]


viewTranslationRecordPreviewButton : DisplayProfile -> TranslationRecord -> Element FrontendMsg
viewTranslationRecordPreviewButton dProfile translationRecord =
    let
        ( inputStyles, translatedStyles ) =
            case translationRecord.translation.translatedTo of
                English ->
                    ( [ Font.italic ], [] )

                Estonian ->
                    ( [], [ Font.italic ] )
    in
    Element.column
        [ Element.padding 10
        , Element.spacing 5
        , Element.width Element.fill
        , Border.rounded 10
        , Border.width 1
        , Border.color <| Element.rgba 0 0 0.5 0.4
        , Element.Background.color <| Element.rgba 0 0 1 0.05
        , Font.size <| responsiveVal dProfile 16 18
        , Element.pointer
        , Events.onClick <| GotoRouteAndAnimate <| Route.View translationRecord.id
        ]
        [ Element.el inputStyles <| textWithCutoff translationRecord.input
        , hbreakElement
        , Element.el translatedStyles <| textWithCutoff translationRecord.translation.translatedText
        ]


loadMoreButton : DisplayProfile -> PublicOrPersonal -> Maybe Int -> Element FrontendMsg
loadMoreButton dProfile publicOrPersonal maybeLowestIdFetched =
    Input.button
        [ Element.paddingXY 20 10
        , Element.Background.color Colors.blue
        , Font.color Colors.white
        , Font.bold
        , Border.rounded 4
        ]
        { onPress = Just <| FetchMoreTranslations publicOrPersonal ( maybeLowestIdFetched, Config.frontendFetchRecordCount )
        , label = Element.text "Load more"
        }


getLowestId : Dict Int TranslationRecord -> Maybe Int
getLowestId =
    Dict.keys
        >> List.head
