module Account.View exposing (..)

import Colors
import CommonView exposing (..)
import Config
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Responsive exposing (..)
import Route
import Types exposing (..)


page : DisplayProfile -> SigninModel -> Maybe FrontendUserInfo -> Element FrontendMsg
page dProfile signinModel maybeUserInfo =
    primaryBox
        [ Element.width Element.fill
        , Element.padding <| responsiveVal dProfile 10 25
        ]
    <|
        case maybeUserInfo of
            Nothing ->
                Element.el [ Element.centerX ] <|
                    signinElement dProfile signinModel

            Just userInfo ->
                Element.column
                    [ Element.centerX
                    , Element.spacing 20
                    ]
                    [ loggedInElement dProfile userInfo
                    , membershipStatusElement dProfile userInfo.membershipStatus
                    ]


loggedInElement : DisplayProfile -> FrontendUserInfo -> Element FrontendMsg
loggedInElement dProfile userInfo =
    Element.column
        [ Element.centerX
        , Element.spacing 10
        ]
        [ Element.text <| "Logged in as " ++ userInfo.email
        , Input.button
            [ Border.rounded 4
            , Element.Background.color Colors.blue
            , Element.paddingXY 20 10
            , Font.color Colors.white
            , Font.bold
            , Element.centerX
            ]
            { onPress = Just Logout
            , label = Element.text "Logout"
            }
        ]


membershipStatusElement : DisplayProfile -> MembershipStatus -> Element FrontendMsg
membershipStatusElement dProfile membershipStatus =
    let
        manageSubscriptionLink =
            Element.newTabLink
                linkAttributes
                { url = Config.stripeUserPortalLink
                , label = Element.text "Manage Subscription"
                }

        ( descriptionEl, actionEl ) =
            let
                gray =
                    Element.rgb 0.3 0.3 0.3

                activeEls =
                    ( Element.el [ Font.color <| Element.rgb 0 0.5 0 ] <| Element.text "active"
                    , manageSubscriptionLink
                    )
            in
            case membershipStatus of
                NoStripeInfo ->
                    ( Element.el [ Font.color gray ] <| Element.text "Not set up"
                    , actionLink "Activate" <| GotoRouteAndAnimate <| Route.Subscribe
                    )

                NotStarted ->
                    ( Element.el [ Font.color gray ] <| Element.text "Not started"
                    , actionLink "Activate" <| GotoRouteAndAnimate <| Route.Subscribe
                    )

                MembershipActive ->
                    activeEls

                MembershipAlmostExpired ->
                    activeEls

                MembershipExpired ->
                    ( Element.el [ Font.color <| Element.rgb 0.5 0 0 ] <| Element.text "Expired"
                    , manageSubscriptionLink
                    )
    in
    Element.column
        [ Element.centerX
        , Element.spacing 10
        , Element.padding 5
        , Element.Background.color <| Element.rgba 0 0 1 0.05
        , Border.color <| Element.rgba 0 0 0 0.3
        , Border.width 1
        , Border.rounded 4
        ]
        [ Element.row
            [ Element.centerX
            ]
            [ Element.text "subscription: "
            , descriptionEl
            ]
        , Element.el [ Element.centerX ] actionEl
        ]
