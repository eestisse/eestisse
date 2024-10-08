module Evergreen.Migrate.V23 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Evergreen.V21.Route
import Evergreen.V21.Types
import Evergreen.V23.Route
import Evergreen.V23.Types
import Lamdera.Migrations exposing (..)
import Result
import Time


frontendModel : Evergreen.V21.Types.FrontendModel -> ModelMigration Evergreen.V23.Types.FrontendModel Evergreen.V23.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V21.Types.BackendModel -> ModelMigration Evergreen.V23.Types.BackendModel Evergreen.V23.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V21.Types.FrontendMsg -> MsgMigration Evergreen.V23.Types.FrontendMsg Evergreen.V23.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V21.Types.ToBackend -> MsgMigration Evergreen.V23.Types.ToBackend Evergreen.V23.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V21.Types.BackendMsg -> MsgMigration Evergreen.V23.Types.BackendMsg Evergreen.V23.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V21.Types.ToFrontend -> MsgMigration Evergreen.V23.Types.ToFrontend Evergreen.V23.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_Types_FrontendModel : Evergreen.V21.Types.FrontendModel -> Evergreen.V23.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , route = old.route |> migrate_Route_Route
    , translationPageModel = old.translationPageModel |> migrate_Types_TranslationPageModel
    , signupState = old.signupState |> migrate_Types_SignupState
    , maybeImportantNumber = old.maybeImportantNumber
    , animationTime = Time.millisToPosix 0
    , backgroundModel = Nothing
    }


migrate_Route_Route : Evergreen.V21.Route.Route -> Evergreen.V23.Route.Route
migrate_Route_Route old =
    case old of
        Evergreen.V21.Route.Translate ->
            Evergreen.V23.Route.Translate

        Evergreen.V21.Route.Landing ->
            Evergreen.V23.Route.Landing

        Evergreen.V21.Route.Admin ->
            Evergreen.V23.Route.Admin

        Evergreen.V21.Route.BadRoute ->
            Evergreen.V23.Route.BadRoute


migrate_Types_BreakdownPart : Evergreen.V21.Types.BreakdownPart -> Evergreen.V23.Types.BreakdownPart
migrate_Types_BreakdownPart old =
    old


migrate_Types_CompletedRequest : Evergreen.V21.Types.CompletedRequest -> Evergreen.V23.Types.CompletedRequest
migrate_Types_CompletedRequest old =
    { inputText = old.inputText
    , translationResult = old.translationResult |> Result.mapError migrate_Types_GptAssistError >> Result.map migrate_Types_Translation
    , maybeSelectedBreakdownPart = old.maybeSelectedBreakdownPart
    }


migrate_Types_EnglishOrEstonian : Evergreen.V21.Types.EnglishOrEstonian -> Evergreen.V23.Types.EnglishOrEstonian
migrate_Types_EnglishOrEstonian old =
    case old of
        Evergreen.V21.Types.English ->
            Evergreen.V23.Types.English

        Evergreen.V21.Types.Estonian ->
            Evergreen.V23.Types.Estonian


migrate_Types_FrontendMsg : Evergreen.V21.Types.FrontendMsg -> Evergreen.V23.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V21.Types.UrlClicked p0 ->
            Evergreen.V23.Types.UrlClicked p0

        Evergreen.V21.Types.UrlChanged p0 ->
            Evergreen.V23.Types.UrlChanged p0

        Evergreen.V21.Types.NoOpFrontendMsg ->
            Evergreen.V23.Types.NoOpFrontendMsg

        Evergreen.V21.Types.TextInputChanged p0 ->
            Evergreen.V23.Types.TextInputChanged p0

        Evergreen.V21.Types.SubmitText p0 ->
            Evergreen.V23.Types.SubmitText p0

        Evergreen.V21.Types.ShowExplanation p0 ->
            Evergreen.V23.Types.ShowExplanation (p0 |> migrate_Types_BreakdownPart)

        Evergreen.V21.Types.CycleLoadingAnimation ->
            Evergreen.V23.Types.CycleLoadingAnimation

        Evergreen.V21.Types.EditTranslation p0 ->
            Evergreen.V23.Types.EditTranslation p0

        Evergreen.V21.Types.GotoRoute p0 ->
            Evergreen.V23.Types.GotoRoute (p0 |> migrate_Route_Route)

        Evergreen.V21.Types.StartSignup ->
            Evergreen.V23.Types.StartSignup

        Evergreen.V21.Types.SubmitSignup p0 ->
            Evergreen.V23.Types.SubmitSignup p0

        Evergreen.V21.Types.SignupTextChanged p0 ->
            Evergreen.V23.Types.SignupTextChanged p0

        Evergreen.V21.Types.FetchImportantNumber ->
            Evergreen.V23.Types.FetchImportantNumber


migrate_Types_GptAssistError : Evergreen.V21.Types.GptAssistError -> Evergreen.V23.Types.GptAssistError
migrate_Types_GptAssistError old =
    case old of
        Evergreen.V21.Types.OutOfCredits ->
            Evergreen.V23.Types.OutOfCredits

        Evergreen.V21.Types.ApiProtocolError p0 ->
            Evergreen.V23.Types.ApiProtocolError p0

        Evergreen.V21.Types.GptDecodeError p0 ->
            Evergreen.V23.Types.GptDecodeError p0

        Evergreen.V21.Types.GptExpressedError p0 ->
            Evergreen.V23.Types.GptExpressedError p0


migrate_Types_RequestState : Evergreen.V21.Types.RequestState -> Evergreen.V23.Types.RequestState
migrate_Types_RequestState old =
    case old of
        Evergreen.V21.Types.Waiting p0 p1 ->
            Evergreen.V23.Types.Waiting p0 p1

        Evergreen.V21.Types.RequestComplete p0 ->
            Evergreen.V23.Types.RequestComplete (p0 |> migrate_Types_CompletedRequest)


migrate_Types_SignupState : Evergreen.V21.Types.SignupState -> Evergreen.V23.Types.SignupState
migrate_Types_SignupState old =
    case old of
        Evergreen.V21.Types.Inactive ->
            Evergreen.V23.Types.Inactive

        Evergreen.V21.Types.Active p0 ->
            Evergreen.V23.Types.Active p0

        Evergreen.V21.Types.Submitting ->
            Evergreen.V23.Types.Submitting

        Evergreen.V21.Types.Submitted ->
            Evergreen.V23.Types.Submitted


migrate_Types_Translation : Evergreen.V21.Types.Translation -> Evergreen.V23.Types.Translation
migrate_Types_Translation old =
    { breakdown = old.breakdown
    , translation = old.translation
    , translatedTo = old.translatedTo |> migrate_Types_EnglishOrEstonian
    }


migrate_Types_TranslationPageModel : Evergreen.V21.Types.TranslationPageModel -> Evergreen.V23.Types.TranslationPageModel
migrate_Types_TranslationPageModel old =
    case old of
        Evergreen.V21.Types.InputtingText p0 ->
            Evergreen.V23.Types.InputtingText p0

        Evergreen.V21.Types.RequestSent p0 ->
            Evergreen.V23.Types.RequestSent (p0 |> migrate_Types_RequestState)
