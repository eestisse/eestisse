module Evergreen.Migrate.V37 exposing (..)

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

import Evergreen.V33.Background.Types
import Evergreen.V33.Responsive
import Evergreen.V33.Route
import Evergreen.V33.Types
import Evergreen.V37.Background.Types
import Evergreen.V37.Point
import Evergreen.V37.Responsive
import Evergreen.V37.Route
import Evergreen.V37.Types
import Lamdera.Migrations exposing (..)
import List
import Maybe
import Result


frontendModel : Evergreen.V33.Types.FrontendModel -> ModelMigration Evergreen.V37.Types.FrontendModel Evergreen.V37.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V33.Types.BackendModel -> ModelMigration Evergreen.V37.Types.BackendModel Evergreen.V37.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V33.Types.FrontendMsg -> MsgMigration Evergreen.V37.Types.FrontendMsg Evergreen.V37.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V33.Types.ToBackend -> MsgMigration Evergreen.V37.Types.ToBackend Evergreen.V37.Types.BackendMsg
toBackend old =
    MsgMigrated ( migrate_Types_ToBackend old, Cmd.none )


backendMsg : Evergreen.V33.Types.BackendMsg -> MsgMigration Evergreen.V37.Types.BackendMsg Evergreen.V37.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V33.Types.ToFrontend -> MsgMigration Evergreen.V37.Types.ToFrontend Evergreen.V37.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_FrontendModel : Evergreen.V33.Types.FrontendModel -> Evergreen.V37.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , route = old.route |> migrate_Route_Route
    , dProfile = old.dProfile |> Maybe.map migrate_Responsive_DisplayProfile
    , translationPageModel = old.translationPageModel |> migrate_Types_TranslationPageModel
    , signupState = old.signupState |> migrate_Types_SignupState
    , maybeImportantNumbers = old.maybeImportantNumbers
    , animationTime = old.animationTime
    , backgroundModel = old.backgroundModel |> Maybe.map migrate_Background_Types_Model
    , publicCredits = Nothing
    , showCreditCounterTooltip = False
    , creditsCounterAnimationState = Nothing
    }


migrate_Background_Types_Model : Evergreen.V33.Background.Types.Model -> Evergreen.V37.Background.Types.Model
migrate_Background_Types_Model old =
    { seed = old.seed
    , animationTime = old.animationTime
    , pathsAcross = old.pathsAcross |> List.map (Tuple.mapBoth migrate_Background_Types_PathAcross (Maybe.map migrate_Background_Types_PathAcrossAnimationState))
    }


migrate_Background_Types_PathAcross : Evergreen.V33.Background.Types.PathAcross -> Evergreen.V37.Background.Types.PathAcross
migrate_Background_Types_PathAcross old =
    { yPathStart = old.yPathStart
    , sections = old.sections |> List.map migrate_Background_Types_PathSection
    , color =
        old.color
            |> (\rec -> rec)
    }


migrate_Background_Types_PathAcrossAnimationState : Evergreen.V33.Background.Types.PathAcrossAnimationState -> Evergreen.V37.Background.Types.PathAcrossAnimationState
migrate_Background_Types_PathAcrossAnimationState old =
    { pathAcrossTarget = old.pathAcrossTarget |> migrate_Background_Types_PathAcross
    , animationStart = old.animationStart
    }


migrate_Background_Types_PathPiece : Evergreen.V33.Background.Types.PathPiece -> Evergreen.V37.Background.Types.PathPiece
migrate_Background_Types_PathPiece old =
    case old of
        Evergreen.V33.Background.Types.ElbowLeftToUp ->
            Evergreen.V37.Background.Types.ElbowLeftToUp

        Evergreen.V33.Background.Types.ElbowLeftToDown ->
            Evergreen.V37.Background.Types.ElbowLeftToDown

        Evergreen.V33.Background.Types.ElbowUpToRight ->
            Evergreen.V37.Background.Types.ElbowUpToRight

        Evergreen.V33.Background.Types.ElbowDownToRight ->
            Evergreen.V37.Background.Types.ElbowDownToRight

        Evergreen.V33.Background.Types.Right p0 ->
            Evergreen.V37.Background.Types.Right p0

        Evergreen.V33.Background.Types.Up p0 ->
            Evergreen.V37.Background.Types.Up p0

        Evergreen.V33.Background.Types.Down p0 ->
            Evergreen.V37.Background.Types.Down p0


migrate_Background_Types_PathSection : Evergreen.V33.Background.Types.PathSection -> Evergreen.V37.Background.Types.PathSection
migrate_Background_Types_PathSection old =
    { piece = old.piece |> migrate_Background_Types_PathPiece
    , endPointRelative = old.endPointRelative |> migrate_Point_Point
    , startPointRelative = old.startPointRelative |> migrate_Point_Point
    }


migrate_Point_Point : Evergreen.V33.Background.Types.Point -> Evergreen.V37.Point.Point
migrate_Point_Point old =
    old


migrate_Responsive_DisplayProfile : Evergreen.V33.Responsive.DisplayProfile -> Evergreen.V37.Responsive.DisplayProfile
migrate_Responsive_DisplayProfile old =
    case old of
        Evergreen.V33.Responsive.Desktop ->
            Evergreen.V37.Responsive.Desktop

        Evergreen.V33.Responsive.Mobile ->
            Evergreen.V37.Responsive.Mobile


migrate_Route_Route : Evergreen.V33.Route.Route -> Evergreen.V37.Route.Route
migrate_Route_Route old =
    case old of
        Evergreen.V33.Route.Translate ->
            Evergreen.V37.Route.Translate

        Evergreen.V33.Route.Landing ->
            Evergreen.V37.Route.Landing

        Evergreen.V33.Route.Admin ->
            Evergreen.V37.Route.Admin

        Evergreen.V33.Route.BadRoute ->
            Evergreen.V37.Route.BadRoute


migrate_Types_BreakdownPart : Evergreen.V33.Types.BreakdownPart -> Evergreen.V37.Types.BreakdownPart
migrate_Types_BreakdownPart old =
    old


migrate_Types_CompletedRequest : Evergreen.V33.Types.CompletedRequest -> Evergreen.V37.Types.CompletedRequest
migrate_Types_CompletedRequest old =
    { inputText = old.inputText
    , translationResult = old.translationResult |> Result.mapError migrate_Types_GptAssistError >> Result.map migrate_Types_Translation
    , maybeSelectedBreakdownPart = old.maybeSelectedBreakdownPart
    }


migrate_Types_EnglishOrEstonian : Evergreen.V33.Types.EnglishOrEstonian -> Evergreen.V37.Types.EnglishOrEstonian
migrate_Types_EnglishOrEstonian old =
    case old of
        Evergreen.V33.Types.English ->
            Evergreen.V37.Types.English

        Evergreen.V33.Types.Estonian ->
            Evergreen.V37.Types.Estonian


migrate_Types_FrontendMsg : Evergreen.V33.Types.FrontendMsg -> Evergreen.V37.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V33.Types.NoOpFrontendMsg ->
            Evergreen.V37.Types.NoOpFrontendMsg

        Evergreen.V33.Types.UrlClicked p0 ->
            Evergreen.V37.Types.UrlClicked p0

        Evergreen.V33.Types.UrlChanged p0 ->
            Evergreen.V37.Types.UrlChanged p0

        Evergreen.V33.Types.GotViewport p0 ->
            Evergreen.V37.Types.GotViewport p0

        Evergreen.V33.Types.Resize p0 p1 ->
            Evergreen.V37.Types.Resize p0 p1

        Evergreen.V33.Types.TextInputChanged p0 ->
            Evergreen.V37.Types.TextInputChanged p0

        Evergreen.V33.Types.SubmitText p0 ->
            Evergreen.V37.Types.SubmitText p0

        Evergreen.V33.Types.ShowExplanation p0 ->
            Evergreen.V37.Types.ShowExplanation (p0 |> migrate_Types_BreakdownPart)

        Evergreen.V33.Types.CycleLoadingAnimation ->
            Evergreen.V37.Types.CycleLoadingAnimation

        Evergreen.V33.Types.EditTranslation p0 ->
            Evergreen.V37.Types.EditTranslation p0

        Evergreen.V33.Types.GotoRoute p0 ->
            Evergreen.V37.Types.GotoRoute (p0 |> migrate_Route_Route)

        Evergreen.V33.Types.GotoTranslate_FocusAndClear ->
            Evergreen.V37.Types.GotoTranslate_FocusAndClear

        Evergreen.V33.Types.StartSignup ->
            Evergreen.V37.Types.StartSignup

        Evergreen.V33.Types.SubmitSignupClicked p0 ->
            Evergreen.V37.Types.SubmitSignupClicked (p0 |> migrate_Types_SignupFormModel)

        Evergreen.V33.Types.SignupFormChanged p0 ->
            Evergreen.V37.Types.SignupFormChanged (p0 |> migrate_Types_SignupFormModel)

        Evergreen.V33.Types.FetchImportantNumber ->
            Evergreen.V37.Types.FetchImportantNumber

        Evergreen.V33.Types.Animate p0 ->
            Evergreen.V37.Types.Animate p0

        Evergreen.V33.Types.FiddleRandomBackroundPath p0 ->
            Evergreen.V37.Types.FiddleRandomBackroundPath p0


migrate_Types_GptAssistError : Evergreen.V33.Types.GptAssistError -> Evergreen.V37.Types.GptAssistError
migrate_Types_GptAssistError old =
    case old of
        Evergreen.V33.Types.OutOfCredits ->
            Evergreen.V37.Types.OutOfCredits

        Evergreen.V33.Types.ApiProtocolError p0 ->
            Evergreen.V37.Types.ApiProtocolError (p0 |> migrate_Types_ProtocolError)

        Evergreen.V33.Types.GptDecodeError p0 ->
            Evergreen.V37.Types.GptDecodeError p0

        Evergreen.V33.Types.GptExpressedError p0 ->
            Evergreen.V37.Types.GptExpressedError p0


migrate_Types_ProtocolError : Evergreen.V33.Types.ProtocolError -> Evergreen.V37.Types.ProtocolError
migrate_Types_ProtocolError old =
    case old of
        Evergreen.V33.Types.RateLimited ->
            Evergreen.V37.Types.RateLimited

        Evergreen.V33.Types.HttpError p0 ->
            Evergreen.V37.Types.HttpError p0


migrate_Types_RequestState : Evergreen.V33.Types.RequestState -> Evergreen.V37.Types.RequestState
migrate_Types_RequestState old =
    case old of
        Evergreen.V33.Types.Waiting p0 p1 ->
            Evergreen.V37.Types.Waiting p0 p1

        Evergreen.V33.Types.RequestComplete p0 ->
            Evergreen.V37.Types.RequestComplete (p0 |> migrate_Types_CompletedRequest)


migrate_Types_SignupFormModel : Evergreen.V33.Types.SignupFormModel -> Evergreen.V37.Types.SignupFormModel
migrate_Types_SignupFormModel old =
    old


migrate_Types_SignupState : Evergreen.V33.Types.SignupState -> Evergreen.V37.Types.SignupState
migrate_Types_SignupState old =
    case old of
        Evergreen.V33.Types.Inactive ->
            Evergreen.V37.Types.Inactive

        Evergreen.V33.Types.Active p0 ->
            Evergreen.V37.Types.Active (p0 |> migrate_Types_SignupFormModel)

        Evergreen.V33.Types.Submitting ->
            Evergreen.V37.Types.Submitting

        Evergreen.V33.Types.Submitted ->
            Evergreen.V37.Types.Submitted


migrate_Types_ToBackend : Evergreen.V33.Types.ToBackend -> Evergreen.V37.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V33.Types.NoOpToBackend ->
            Evergreen.V37.Types.NoOpToBackend

        Evergreen.V33.Types.SubmitTextForTranslation p0 ->
            Evergreen.V37.Types.SubmitTextForTranslation p0

        Evergreen.V33.Types.SubmitSignup p0 ->
            Evergreen.V37.Types.SubmitSignup (p0 |> migrate_Types_SignupFormModel)

        Evergreen.V33.Types.RequestImportantNumber ->
            Evergreen.V37.Types.RequestImportantNumber


migrate_Types_ToFrontend : Evergreen.V33.Types.ToFrontend -> Evergreen.V37.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V33.Types.NoOpToFrontend ->
            Evergreen.V37.Types.NoOpToFrontend

        Evergreen.V33.Types.TranslationResult p0 p1 ->
            Evergreen.V37.Types.TranslationResult p0
                (p1 |> Result.mapError migrate_Types_GptAssistError >> Result.map migrate_Types_Translation)

        Evergreen.V33.Types.EmailSubmitAck ->
            Evergreen.V37.Types.EmailSubmitAck

        Evergreen.V33.Types.ImportantNumbers p0 ->
            Evergreen.V37.Types.ImportantNumbers p0


migrate_Types_Translation : Evergreen.V33.Types.Translation -> Evergreen.V37.Types.Translation
migrate_Types_Translation old =
    { breakdown = old.breakdown
    , translation = old.translation
    , translatedTo = old.translatedTo |> migrate_Types_EnglishOrEstonian
    }


migrate_Types_TranslationPageModel : Evergreen.V33.Types.TranslationPageModel -> Evergreen.V37.Types.TranslationPageModel
migrate_Types_TranslationPageModel old =
    case old of
        Evergreen.V33.Types.InputtingText p0 ->
            Evergreen.V37.Types.InputtingText p0

        Evergreen.V33.Types.RequestSent p0 ->
            Evergreen.V37.Types.RequestSent (p0 |> migrate_Types_RequestState)
