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
    , systemMessage = """You are helping me learn Estonian, by translating between Estonian and English, and explaining the resulting translation. Here's the structure I want you to output, after I send you a piece of English or Estonian text:

{
    "error": false
    "translation": TRANSLATION_HERE,
    "translated to": ("estonian" OR "english")
    "breakdown": [
        [ESTONIAN_CHUNK_1, DIRECT_APPROXIMATE_TRANSLATION_1, EXPLANATION_1],
        [ESTONIAN_CHUNK_2, DIRECT_APPROXIMATE_TRANSLATION_2, EXPLANATION_2],
        ...
    ]
}

The field "translated to" should be either "estonian" or "english" depending on which translation you performed.

The breakdown should consist of at most 10 items, each of which examines a section of the text (either a word or phrase). The explanation should be a list of 3 items: the Estonian chunk, a direct approximate translation of the chunk, and an explanation (null otherwise).

Make sure that the breakdown covers the full input text without missing any words.

If you are unable to complete the request, instead create a json object with "error" set to true and "error_text" set to a string explanation of what seems wrong with the request. Assume a playful tone with the explanation, but make sure it's clear enough to be understood.

In the case of explicit messages, if the messsage features clear communication, do not refuse to translate it. If on the other hand it uses terms that are overly explicit or dirty, deny that request (again, with a clear if playful explanation of why)"""
    , messages =
        [ ( User, """Mind on treenitud kosmoseõpilasi abistama""" )
        , ( Assistant, """{
    "error": false,
    "translation": "I have been trained to assist space students.",
    "translated to": "english",
    "breakdown": [
        [
            "Mind",
            "Me",
            "first person singular pronoun in the partitive case, often used as the object of a verb"
        ],
        [
            "on",
            "has/have been",
            "third person singular present indicative of \\"olema,\\" used here to form a passive voice"
        ],
        [
            "treenitud",
            "trained",
            "past participle of \\"treenima\\")"
        ],
        [
            "kosmoseõpilasi",
            "space students",
            "partitive plural of \\"kosmoseõpilane,\\" a compound noun made from \\"kosmos\\" meaning \\"space\\" and \\"õpilane\\" meaning \\"student\\""
        ],
        [
            "abistama",
            "to assist",
            "infinitive form of \\"abistama\\""
        ]
    ]
}""" )
        , ( User, "Mentally a green thumb, but all the plants at home are half-dead" )
        , ( Assistant, """{
    "error": false,
    "translation": "Mentaalselt rohenäpp, ent kõik taimed kodus on pooleldi surnud",
    "translated to": "estonian",
    "breakdown": [
        [
            "Mentaalselt",
            "Mentally",
            null
        ],
        [
            "rohenäpp",
            "Green thumb",
            "noun, a metaphorical expression used to describe someone who is good at gardening and taking care of plants"
        ],
        [
            "ent",
            "but",
            null
        ],
        [
            "kõik",
            "all",
            null
        ],
        [
            "taimed",
            "plants",
            "plural of \\"taim\\""
        ],
        [
            "kodus",
            "at home",
            "inessive case of \\"kodu\\", indicating location within a home"
        ],
        [
            "on",
            "are",
            "third person plural present indicative of \\"olema\\", the verb \\"to be\\""
        ],
        [
            "pooleldi",
            "half",
            null
        ],
        [
            "surnud",
            "dead",
            "nominative plural past participle of \\"surema\\", meaning \\"to die\\""
        ]
    ]
}""" )
        , ( User, "Watashi wa boku" )
        , ( Assistant, """{"error": true, "error_text": "That looks like Japanese, not Estonian!"}""" )
        , ( User, "ma ei jõua ära oodata, et sa mu vittu lakuks ja ajud välja nussiks" )
        , ( Assistant, """{"error": true, "error_text": "Woah, that input is too explicit. I'm not translating that!"}""" )
        , ( User, "ma tahan sinuga väga seksida. kas sul on kondoome on?" )
        , ( Assistant, """{
    "error": false,
    "translation": "I really want to have sex with you. Do you have condoms?",
    "translated to": "english",
    "breakdown": [
        [
            "ma tahan",
            "I want",
            "first person singular present indicative of \\"tahtma,\\" meaning \\"to want\\"."
        ],
        [
            "sinuga",
            "with you",
            "comitative case of \\"sina\\", meaning \\"you\\", indicating \\"with you\\"."
        ],
        [
            "väga",
            "very",
            null
        ],
        [
            "seksida",
            "to have sex",
            "infinitive form of \\"seksima\\", meaning \\"to have sex\\"."
        ],
        [
            "kas sul on",
            "do you have",
            "\\"kas\\" introduces a question, \\"sul on\\" is the form of \\"to have\\" for \\"you\\"."
        ],
        [
            "kondoome on?",
            "are there condoms?",
            "\\"kondoome\\" is the partitive plural of \\"kondoom\\", meaning \\"condoms\\", and \\"on\\" is \\"are\\"."
        ]
    ]
}""" )
        , ( User, "jfdkslfjaiofjdsafjsafkdla" )
        , ( Assistant, """{"error": true, "error_text": "Now what do you expect me to do with that gobbledegook?"}""" )
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
    Json.Decode.map3
        Translation
        (Json.Decode.field "breakdown"
            (Json.Decode.list
                (Utils.decode3Tuple
                    Json.Decode.string
                    Json.Decode.string
                    (Json.Decode.nullable Json.Decode.string)
                    |> Json.Decode.map
                        (Utils.map3TupleTo BreakdownPart)
                )
            )
        )
        (Json.Decode.field "translation" Json.Decode.string)
        (Json.Decode.field "translated to" englishOrEstonianDecoder)


englishOrEstonianDecoder : Json.Decode.Decoder EnglishOrEstonian
englishOrEstonianDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                if s == "english" then
                    Json.Decode.succeed English

                else if s == "estonian" then
                    Json.Decode.succeed Estonian

                else
                    Json.Decode.fail "'translate_to' field did not include an expected string of 'english' or 'estonian'"
            )


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
