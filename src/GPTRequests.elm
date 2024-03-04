module GPTRequests exposing (..)

import Http
import Json.Decode
import Json.Encode
import Types exposing (..)
import Utils


type alias Request =
    { modelString : String
    , systemMessage : String
    , messages : List ( Role, String )
    }


type Role
    = Assistant
    | User


translateFromEstonian : String -> Request
translateFromEstonian estonianString =
    { modelString = "gpt-4-turbo-preview"
    , systemMessage = """You are helping me learn Estonian, by translating small pieces of Estonian text and explaining the resulting translation. Here's the structure I want you to output, after I send you a piece of Estonian text:

{
    "error": false
    "translation": TRANSLATION_HERE,
    "breakdown": [
        [ESTONIAN_CHUNK_1, EXPLANATION_1],
        [ESTONIAN_CHUNK_2, EXPLANATION_2],
        ...
    ]
}

The breakdown should consist of at most 10 items, each of which examines a section of the text (either a word or phrase). The explanation should be a direct approximate translation of the chunk, and if needed, explanation in parentheses.

Make sure that the breakdown covers the full input text without missing any words.

If you are unable to complete the request, instead create a json object with "error" set to true and "error_text" set to a string explanation of what seems wrong with the request."""
    , messages =
        [ ( User, """Mind on treenitud kosmoseõpilasi abistama""" )
        , ( Assistant, """{
    "error": false,
    "translation": "I have been trained to assist space students.",
    "breakdown": [
        [
            "Mind",
            "Me (first person singular pronoun in the partitive case, often used as the object of a verb)"
        ],
        [
            "on",
            "has/have been (third person singular present indicative of "olema," used here to form a passive voice)"
        ],
        [
            "treenitud",
            "trained (past participle of "treenima")"
        ],
        [
            "kosmoseõpilasi",
            "space students (partitive plural of "kosmoseõpilane," a compound noun made from "kosmos" meaning "space" and "õpilane" meaning "student")"
        ],
        [
            "abistama",
            "to assist (infinitive form of "abistama")"
        ]
    ]
}""" )
        , ( User, "Mentaalselt rohenäpp, ent kõik taimed kodus on pooleldi surnud" )
        , ( Assistant, """{
    "error": false,
    "translation": "Mentally a green thumb, but all the plants at home are half-dead.",
    "breakdown": [
        [
            "Mentaalselt",
            "Mentally (adverb)"
        ],
        [
            "rohenäpp",
            "Green thumb (noun, a metaphorical expression used to describe someone who is good at gardening and taking care of plants)"
        ],
        [
            "ent",
            "but"
        ],
        [
            "kõik",
            "all"
        ],
        [
            "taimed",
            "plants (plural of \"taim\")"
        ],
        [
            "kodus",
            "at home (inessive case of \"kodu\", indicating location within a home)."
        ],
        [
            "on",
            "are (third person plural present indicative of \"olema\", the verb \"to be\")."
        ],
        [
            "pooleldi",
            "half"
        ],
        [
            "surnud",
            "dead (nominative plural past participle of \"surema\", meaning \"to die\")"
        ]
    ]
}""" )
        , ( User, "Watashi wa boku" )
        , ( Assistant, """{"error": true, "error_text": "That looks like Japanese, not Estonian!"}""" )
        , ( User, estonianString )
        ]
    }


encode : Request -> Json.Encode.Value
encode request =
    Json.Encode.object
        [ ( "model", Json.Encode.string request.modelString )
        , ( "messages"
          , Json.Encode.list Json.Encode.object <|
                [ ( "role", Json.Encode.string "system" )
                , ( "content", Json.Encode.string request.systemMessage )
                ]
                    :: (request.messages
                            |> List.map
                                (\( role, message ) ->
                                    [ ( "role", Json.Encode.string <| roleToString role )
                                    , ( "content", Json.Encode.string message )
                                    ]
                                )
                       )
          )
        ]


roleToString : Role -> String
roleToString role =
    case role of
        Assistant ->
            "assistant"

        User ->
            "user"


apiResponseDecoder : Json.Decode.Decoder String
apiResponseDecoder =
    Json.Decode.field "choices"
        (Json.Decode.index 0
            (Json.Decode.field "message" (Json.Decode.field "content" Json.Decode.string))
        )


gptResponseDecoder : Json.Decode.Decoder (Result String Translation)
gptResponseDecoder =
    Json.Decode.field "error" Json.Decode.bool
        |> Json.Decode.andThen
            (\errorBool ->
                if errorBool then
                    Json.Decode.field "error_text" Json.Decode.string
                        |> Json.Decode.map Err

                else
                    translationDecoder |> Json.Decode.map Ok
            )


translationDecoder : Json.Decode.Decoder Translation
translationDecoder =
    Json.Decode.map2
        freshTranslation
        (Json.Decode.field "breakdown"
            (Json.Decode.list
                (Utils.decodeTuple Json.Decode.string Json.Decode.string)
            )
        )
        (Json.Decode.field "translation" Json.Decode.string)


freshTranslation : List ( String, String ) -> String -> Translation
freshTranslation a b =
    Translation a b Nothing


processGptResponse : Result Http.Error String -> Result GptAssistError Translation
processGptResponse fetchResult =
    case fetchResult of
        Err httpErr ->
            Err <| ApiProtocolError httpErr

        Ok gptResponse ->
            case Json.Decode.decodeString gptResponseDecoder gptResponse of
                Err decodeErr ->
                    Err <| GptDecodeError <| Json.Decode.errorToString decodeErr

                Ok (Err gptExpressedError) ->
                    Err <| GptExpressedError gptExpressedError

                Ok (Ok translation) ->
                    Ok translation
