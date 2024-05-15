module StripeLinkback exposing (..)

import CommonView exposing (..)
import Config
import Element exposing (Element)
import Element.Font as Font
import Responsive exposing (..)
import Route
import Types exposing (..)


viewPage : DisplayProfile -> Maybe FrontendUserInfo -> Element FrontendMsg
viewPage dProfile maybeUserInfo =
    primaryBox
        [ Element.width Element.fill
        , Element.padding <| responsiveVal dProfile 10 25
        ]
    <|
        case maybeUserInfo of
            Nothing ->
                Element.row [ Element.centerX ]
                    [ Element.text "Can't find your user id. Try "
                    , actionLink "logging in again" <| GotoRouteAndAnimate Route.Account
                    ]

            Just userInfo ->
                let
                    nonSuccessEl text =
                        Element.paragraph [ Element.centerX ] [ Element.text text ]
                in
                case userInfo.membershipStatus of
                    NoStripeInfo ->
                        nonSuccessEl "We're not seeing any Stripe activity on our end. If you've paid with Stripe, please reach out at admin@eestisse.ee."

                    NotStarted ->
                        nonSuccessEl "We see the Stripe signup, but the payment has not been marked as completed. If you've paid and this message persists, please reach out at admin@eestisse.ee."

                    MembershipActive ->
                        susbcribeSuccessfulElement dProfile

                    MembershipAlmostExpired ->
                        susbcribeSuccessfulElement dProfile

                    MembershipExpired ->
                        Element.newTabLink
                            (linkAttributes ++ [ Element.centerX ])
                            { url = Config.stripeUserPortalLink
                            , label = Element.text "Manage Subscription"
                            }


susbcribeSuccessfulElement : DisplayProfile -> Element FrontendMsg
susbcribeSuccessfulElement dProfile =
    Element.column
        [ Element.centerX
        , Element.spacing 10
        ]
        [ Element.el
            [ Font.size <| responsiveVal dProfile 20 26
            , Element.centerX
            ]
          <|
            Element.text "Subscription active!"
        , Element.paragraph [ Element.centerX ]
            [ Element.text <|
                responsiveVal dProfile
                    "Use the menu button on the upper left to try out different features."
                    "Use the menu on the left to try out different features."
            ]
        ]
