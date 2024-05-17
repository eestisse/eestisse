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


page : DisplayProfile -> SigninModel -> Maybe ConsentsFormModel -> Maybe (Maybe FrontendUserInfo) -> Element FrontendMsg
page dProfile signinModel maybeConsentsFormModel maybeMaybeUserInfo =
    primaryBox
        [ Element.width Element.fill
        , Element.padding <| responsiveVal dProfile 10 25
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.spacing <| responsiveVal dProfile 15 30
            ]
        <|
            case maybeMaybeUserInfo of
                Nothing ->
                    [ Element.el [ Element.centerX ] <| Element.text "Loading..." ]

                Just Nothing ->
                    [ Element.el [ Element.centerX ] <| signinElement dProfile signinModel ]

                Just (Just userInfo) ->
                    loggedInElement dProfile userInfo
                        :: (if not userInfo.consentsSubmitted then
                                [ Element.el [ Element.centerX ] <| viewConsentsForm dProfile maybeConsentsFormModel ]

                            else
                                let
                                    manageSubscriptionLink =
                                        Element.newTabLink
                                            linkAttributes
                                            { url = Config.stripeUserPortalLink
                                            , label = Element.text "Manage Subscription"
                                            }
                                in
                                case userInfo.membershipStatus of
                                    NoStripeInfo ->
                                        [ Element.el [ Element.centerX ] <| viewOffer dProfile
                                        , Element.el [ Element.centerX ] <| purchaseButton dProfile userInfo
                                        ]

                                    NotStarted ->
                                        [ Element.el [ Element.centerX ] <| viewOffer dProfile
                                        , Element.el [ Element.centerX ] <|
                                            Element.newTabLink
                                                linkAttributes
                                                { url = Config.stripeUserPortalLink
                                                , label = Element.text "Waiting for Stripe payment"
                                                }
                                        ]

                                    MembershipActive ->
                                        [ Element.el [ Element.centerX ] <|
                                            membershipStatusElement
                                                dProfile
                                                (Element.el [ Font.color <| Element.rgb 0 0.5 0 ] <| Element.text "active")
                                                manageSubscriptionLink
                                        ]

                                    MembershipAlmostExpired ->
                                        [ Element.el [ Element.centerX ] <|
                                            membershipStatusElement
                                                dProfile
                                                (Element.el [ Font.color <| Element.rgb 0 0.5 0 ] <| Element.text "active")
                                                manageSubscriptionLink
                                        ]

                                    MembershipExpired ->
                                        [ Element.el [ Element.centerX ] <|
                                            membershipStatusElement
                                                dProfile
                                                (Element.el [ Font.color <| Element.rgb 0.5 0 0 ] <| Element.text "Expired")
                                                manageSubscriptionLink
                                        ]
                           )


loggedInElement : DisplayProfile -> FrontendUserInfo -> Element FrontendMsg
loggedInElement dProfile userInfo =
    case dProfile of
        Mobile ->
            Element.row
                [ Element.width Element.fill ]
                [ Element.column
                    []
                    [ Element.text "Logged in:"
                    , Element.text userInfo.email
                    ]
                , lightBlueButton dProfile [ Element.alignRight ] [] "Logout" (Just Logout)
                ]

        Desktop ->
            Element.row
                [ Element.width Element.fill
                , Element.spacing 10
                ]
                [ Element.el [ Element.width Element.fill ] Element.none
                , Element.row
                    [ Element.centerX ]
                    [ Element.text "Logged in: "
                    , Element.text userInfo.email
                    ]
                , Element.el [ Element.width Element.fill ] <|
                    lightBlueButton dProfile
                        [ Element.alignRight ]
                        []
                        "Logout"
                        (Just Logout)
                ]


membershipStatusElement : DisplayProfile -> Element FrontendMsg -> Element FrontendMsg -> Element FrontendMsg
membershipStatusElement dProfile descriptionEl actionEl =
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


viewConsentsForm : DisplayProfile -> Maybe ConsentsFormModel -> Element FrontendMsg
viewConsentsForm dProfile maybeConsentsFormModel =
    let
        consentsFormModel =
            maybeConsentsFormModel
                |> Maybe.withDefault (ConsentsFormModel False False)
    in
    Element.column
        [ Element.spacing 10
        , Element.padding <| responsiveVal dProfile 5 10
        , Element.Background.color <| Element.rgb 0.95 0.95 1
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 8
            , color = Element.rgb 0.8 0.8 0.8
            }
        , Border.width 1
        , Border.color <| Element.rgb 0.7 0.7 1
        , Border.rounded 5
        ]
        [ Element.el [ Font.size <| responsiveVal dProfile 18 20 ] <|
            Element.text "I am interested in..."
        , consentCheckbox dProfile Config.newFeaturesConsentWording consentsFormModel.features (\b -> { consentsFormModel | features = b })
        , consentCheckbox dProfile Config.userInterviewsConsentWording consentsFormModel.interview (\b -> { consentsFormModel | interview = b })
        , blueButton
            dProfile
            [ Element.centerX ]
            []
            "Continue"
            (Just <| ConsentsFormSubmitClicked consentsFormModel)
        ]


consentCheckbox : DisplayProfile -> String -> Bool -> (Bool -> ConsentsFormModel) -> Element FrontendMsg
consentCheckbox dProfile text checked formUpdater =
    Input.checkbox
        []
        { onChange = formUpdater >> ConsentsFormChanged
        , icon = Input.defaultCheckbox
        , checked = checked
        , label =
            Input.labelRight
                [ Element.paddingEach
                    { left = 8
                    , right = 0
                    , bottom = 0
                    , top = 0
                    }
                , Font.size <| responsiveVal dProfile 16 18
                , Element.width Element.fill
                ]
            <|
                Element.paragraph [ Element.spacing 2 ] [ Element.text text ]
        }
