module Subscribe.View exposing (..)

import Auth.Common exposing (ToBackend(..))
import Colors
import CommonView exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Responsive exposing (..)
import Types exposing (..)


page : DisplayProfile -> FrontendModel -> Element FrontendMsg
page dProfile model =
    primaryBox
        [ Element.centerX
        , Element.width Element.fill
        ]
    <|
        Element.column
            [ Element.centerX
            , Element.padding <| responsiveVal dProfile 10 20
            , Element.spacing <| responsiveVal dProfile 15 30
            ]
            [ if maybeFrontendUserHasActiveMembership model.maybeAuthedUserInfo then
                susbcribeSuccessfulElement dProfile

              else
                Element.el
                    [ Font.size <| responsiveVal dProfile 20 26
                    , Element.centerX
                    , Font.bold
                    ]
                <|
                    Element.text "Activate Eestisse subscription"
            , Element.column
                [ Element.centerX
                , Element.spacing <| responsiveVal dProfile 15 30
                ]
                [ Element.el [ Element.centerX ] <| viewOffer dProfile
                , if not <| maybeFrontendUserHasActiveMembership model.maybeAuthedUserInfo then
                    case model.maybeAuthedUserInfo of
                        Nothing ->
                            signinElement dProfile model.signinModel

                        Just userInfo ->
                            purchaseButton dProfile userInfo

                  else
                    Element.none
                ]
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
        , Element.spacing 20
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
        , Element.paragraph
            [ Font.size <| responsiveVal dProfile 12 14 ]
            [ Element.row []
                [ Element.text "This is a discounted price for early supporters. Super Earlybird subscriptions will never increase in price as long as they remain active."
                ]
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
            [ Element.paragraph [] [ Element.text "Unlimited* translations" ]
            , Element.paragraph [] [ Element.text "Option to translate privately" ]
            , Element.paragraph [] [ Element.text "Personal translation history" ]
            , Element.paragraph [] [ Element.text "Access to all upcoming premium features" ]
            ]
        , Element.paragraph
            [ Font.size <| responsiveVal dProfile 8 10
            , Font.color <| Element.rgb 0.3 0.3 0.3
            ]
            [ Element.text "* Eestisse App OÜ may in the future impose some limits on translation use, if the backend cost of the service becomes prohibitive due to exploitation or unexpected use." ]
        ]


purchaseButton : DisplayProfile -> FrontendUserInfo -> Element FrontendMsg
purchaseButton dProfile userInfo =
    Input.button
        [ Element.Background.color <| Element.rgb255 0 116 212
        , Font.size 18
        , Font.extraBold
        , Font.color <| Colors.white
        , Element.centerX
        , Border.rounded 5
        , Element.height <| Element.px 50
        , Element.width <| Element.px 250
        ]
        { onPress = Just <| TriggerStripePayment userInfo.id
        , label = Element.el [ Element.centerX ] <| Element.text "Checkout with Stripe"
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
        , Element.el [ Element.centerX ] <|
            Element.text <|
                responsiveVal dProfile
                    "Use the menu button on the upper left to try out different features."
                    "Use the menu on the left to try out different features."
        ]
