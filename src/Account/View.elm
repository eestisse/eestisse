module Account.View exposing (..)

import Auth.Common exposing (ToBackend(..))
import CommonView exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Responsive exposing (..)
import Types exposing (..)


page : DisplayProfile -> FrontendModel -> Element FrontendMsg
page dProfile model =
    primaryBox
        [ Element.centerX
        , Element.width Element.fill
        ]
    <|
        if maybeFrontendUserHasActiveMembership model.maybeAuthedUserInfo then
            Element.text "oh nice brudda nice wan"

        else
            viewOfferAndActionButtons dProfile model.maybeAuthedUserInfo


viewOfferAndActionButtons : DisplayProfile -> Maybe FrontendUserInfo -> Element FrontendMsg
viewOfferAndActionButtons dProfile maybeAuthedUserInfo =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.el [ Element.centerX ] <| viewOffer dProfile
        , Element.el [ Element.centerX ] <| viewAccountInfoAndActionButton dProfile maybeAuthedUserInfo
        ]


viewOffer : DisplayProfile -> Element FrontendMsg
viewOffer dProfile =
    Element.column
        [ Element.width <| responsiveVal dProfile Element.fill (Element.px 500)
        , Element.Background.color <| Element.rgb 0.8 1 0.8
        , Border.rounded 15
        , Border.width 1
        , Border.color <| Element.rgb 0.6 0.8 0.6
        , Element.padding 20
        , Element.spacing 25
        , Font.size <| responsiveVal dProfile 18 22
        , basicShadow
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing <| responsiveVal dProfile 20 30
            ]
            [ Element.el [ Font.bold ] <| Element.text "Super Earlybird Special"
            , Element.el [ Element.alignRight ] <| Element.text "€3/month"
            ]
        , bulletPointList
            (responsiveVal dProfile 14 18)
            [ Element.paddingEach
                { left = 10
                , right = 0
                , bottom = 0
                , top = 0
                }
            , Font.size <| responsiveVal dProfile 14 18
            ]
            [ Element.paragraph [] [ Element.text "5000 personal translation credits per month" ]
            , Element.paragraph [] [ Element.text "Option to translate privately" ]
            , Element.paragraph [] [ Element.text "Access to new features when they launch (without the price increase!)" ]
            ]
        ]


viewAccountInfoAndActionButton : DisplayProfile -> Maybe FrontendUserInfo -> Element FrontendMsg
viewAccountInfoAndActionButton dProfile maybeAuthedUserInfo =
    case maybeAuthedUserInfo of
        Nothing ->
            mainActionButton "login with google oauth" <|
                Just <|
                    AuthSigninRequested { methodId = "OAuthGoogle", username = Nothing }

        Just userInfo ->
            Element.column
                [ Element.centerX
                , Element.spacing 10
                ]
                [ Element.text <| "Logged in as " ++ userInfo.email
                , mainActionButton "Buy with Stripe" <| Just <| TriggerStripePayment userInfo.id
                ]
