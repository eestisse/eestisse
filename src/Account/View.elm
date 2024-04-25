module Account.View exposing (..)

import Auth.Common exposing (ToBackend(..))
import CommonView exposing (..)
import Element exposing (Element)
import Responsive exposing (..)
import Types exposing (..)


page : DisplayProfile -> FrontendModel -> Element FrontendMsg
page dProfile model =
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
        [ viewOffer dProfile
        , viewAccountInfoAndActionButton dProfile maybeAuthedUserInfo
        ]


viewOffer : DisplayProfile -> Element FrontendMsg
viewOffer dProfile =
    Element.text "how about please give me money"


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
