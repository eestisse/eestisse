module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.


type Mode
    = Development
    | Production


anthropicApiKey : String
anthropicApiKey =
    ""


openaiApiKey : String
openaiApiKey =
    ""


googleAppClientId : String
googleAppClientId =
    ""


googleAppClientSecret : String
googleAppClientSecret =
    ""


stripeWebhookSecret : String
stripeWebhookSecret =
    ""


mode =
    Development
