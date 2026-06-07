module Eval.Json exposing (processor, encodeList, encodeObject, jsonEncode, parseJson, runDecoder)

{-| The editor interpreter's JSON layer: a hand-rolled JSON parser/serialiser (`parseJson`,
`jsonEncode`) and the `Json.Decode`/`Json.Encode` interpreter (`runDecoder`, `encodeList`/
`encodeObject`). A self-contained codec; it needs the evaluator only to apply decoder/encoder
functions, so `applyValue` is passed in as a parameter (`ApplyTo`) rather than importing `Eval`.

It also exposes a {@link Eval.Core.Processor} for the decoder-building builtins (`field`/`succeed`/
`map2`/…), which construct `VCtor "Dec.*"` decoder values that `runDecoder` later interprets. -}

import Eval.Core exposing (Core, Processor)
import Lang exposing (Globals, Value(..))


{-| The `Json.Decode` builtins, as a {@link Eval.Core.Processor}: they build decoder values (the
`runDecoder` interpreter below evaluates them against parsed JSON). All pure. -}
processor : Processor
processor =
    { names = decoderNames
    , arities = decoderArities
    , run = runDecoderBuiltin
    }


decoderNames : List String
decoderNames =
    [ "field", "at", "map", "oneOrMore", "succeed", "map2", "map3", "map4", "map5", "map6", "map7", "map8", "list", "andThen", "oneOf", "nullable" ]


decoderArities : List ( Int, List String )
decoderArities =
    [ ( 1, [ "succeed", "list", "oneOf", "nullable" ] )
    , ( 3, [ "map2" ] )
    , ( 4, [ "map3" ] )
    , ( 5, [ "map4" ] )
    , ( 6, [ "map5" ] )
    , ( 7, [ "map6" ] )
    , ( 8, [ "map7" ] )
    , ( 9, [ "map8" ] )
    ]


runDecoderBuiltin : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
runDecoderBuiltin _ _ name args =
    case ( name, args ) of
        ( "field", [ VStr fieldName, decoder ] ) ->
            Just (Ok (VCtor "Dec.field" [ VStr fieldName, decoder ]))

        ( "at", [ VList path, decoder ] ) ->
            -- `at [ "a", "b" ] dec` is sugar for nested fields: field "a" (field "b" dec).
            Just
                (Ok
                    (List.foldr
                        (\seg acc ->
                            case seg of
                                VStr fieldName ->
                                    VCtor "Dec.field" [ VStr fieldName, acc ]

                                _ ->
                                    acc
                        )
                        decoder
                        path
                    )
                )

        ( "map", [ f, dec ] ) ->
            Just (Ok (VCtor "Dec.map" [ f, dec ]))

        ( "oneOrMore", [ f, dec ] ) ->
            Just (Ok (VCtor "Dec.oneOrMore" [ f, dec ]))

        ( "succeed", [ v ] ) ->
            Just (Ok (VCtor "Dec.succeed" [ v ]))

        ( "map2", [ f, a, b ] ) ->
            Just (Ok (VCtor "Dec.map" [ f, a, b ]))

        ( "map3", [ f, a, b, c ] ) ->
            Just (Ok (VCtor "Dec.map" [ f, a, b, c ]))

        ( "map4", [ f, a, b, c, d ] ) ->
            Just (Ok (VCtor "Dec.map" [ f, a, b, c, d ]))

        ( "map5", [ f, a, b, c, d, e ] ) ->
            Just (Ok (VCtor "Dec.map" [ f, a, b, c, d, e ]))

        ( "map6", [ f, a, b, c, d, e, g ] ) ->
            Just (Ok (VCtor "Dec.map" [ f, a, b, c, d, e, g ]))

        ( "map7", [ f, a, b, c, d, e, g, h ] ) ->
            Just (Ok (VCtor "Dec.map" [ f, a, b, c, d, e, g, h ]))

        ( "map8", [ f, a, b, c, d, e, g, h, i ] ) ->
            Just (Ok (VCtor "Dec.map" [ f, a, b, c, d, e, g, h, i ]))

        ( "list", [ dec ] ) ->
            Just (Ok (VCtor "Dec.list" [ dec ]))

        ( "andThen", [ f, dec ] ) ->
            Just (Ok (VCtor "Dec.andThen" [ f, dec ]))

        ( "oneOf", [ decs ] ) ->
            Just (Ok (VCtor "Dec.oneOf" [ decs ]))

        ( "nullable", [ dec ] ) ->
            Just (Ok (VCtor "Dec.nullable" [ dec ]))

        _ ->
            Nothing


{-| The evaluator's `applyValue`, injected by `Eval` to avoid an import cycle. -}
type alias ApplyTo =
    Globals -> Value -> Value -> Result String Value


{-| Assoc-list lookup (a local copy so this module needn't import Eval). -}
lookup : String -> List ( String, a ) -> Maybe a
lookup name pairs =
    case pairs of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if k == name then
                Just v

            else
                lookup name rest


runDecoder : ApplyTo -> Globals -> Value -> Value -> Result String Value
runDecoder applyValue globals decoder json =
    case decoder of
        VCtor "Dec.file" [] ->
            -- The editor can't materialise a real File from JSON; yield a placeholder so a decoder
            -- that mentions File.decoder still runs (real File drops aren't wired in the editor).
            Ok (VCtor "File" [ VStr "file", VStr "" ])

        VCtor "Dec.string" [] ->
            case json of
                VStr s ->
                    Ok (VStr s)

                _ ->
                    Err "expected a string"

        VCtor "Dec.int" [] ->
            case json of
                VNum n ->
                    Ok (VNum n)

                _ ->
                    Err "expected an int"

        VCtor "Dec.float" [] ->
            case json of
                VNum n ->
                    Ok (VNum n)

                _ ->
                    Err "expected a float"

        VCtor "Dec.bool" [] ->
            case json of
                VBool b ->
                    Ok (VBool b)

                _ ->
                    Err "expected a bool"

        VCtor "Dec.field" [ VStr name, dec ] ->
            case json of
                VRecord fs ->
                    case lookup name fs of
                        Just v ->
                            runDecoder applyValue globals dec v

                        Nothing ->
                            Err ("no field: " ++ name)

                _ ->
                    Err "expected an object"

        VCtor "Dec.succeed" [ v ] ->
            Ok v

        VCtor "Dec.map" (f :: decs) ->
            decodeAll applyValue globals decs json [] |> Result.andThen (\vals -> applyAll applyValue globals f vals)

        VCtor "Dec.list" [ dec ] ->
            case json of
                VList items ->
                    decodeEach applyValue globals dec items []

                _ ->
                    Err "expected a list"

        VCtor "Dec.andThen" [ f, dec ] ->
            runDecoder applyValue globals dec json
                |> Result.andThen
                    (\v ->
                        applyValue globals f v
                            |> Result.andThen (\next -> runDecoder applyValue globals next json)
                    )

        VCtor "Dec.oneOrMore" [ f, dec ] ->
            -- A non-empty JSON array: decode each element, then apply f to (head, tail).
            case json of
                VList (first :: rest) ->
                    runDecoder applyValue globals dec first
                        |> Result.andThen
                            (\head ->
                                decodeEach applyValue globals dec rest []
                                    |> Result.andThen (\tail -> applyAll applyValue globals f [ head, tail ])
                            )

                VList [] ->
                    Err "expected a non-empty list"

                _ ->
                    Err "expected a list"

        VCtor "Dec.oneOf" [ VList decs ] ->
            tryDecoders applyValue globals decs json

        VCtor "Dec.nullable" [ dec ] ->
            case json of
                VCtor "Null" [] ->
                    Ok (VCtor "Nothing" [])

                _ ->
                    runDecoder applyValue globals dec json |> Result.map (\v -> VCtor "Just" [ v ])

        _ ->
            Err "unsupported decoder"


{-| Runs `dec` against each element of a JSON array, collecting the decoded values into a `VList`. -}
decodeEach : ApplyTo -> Globals -> Value -> List Value -> List Value -> Result String Value
decodeEach applyValue globals dec items acc =
    case items of
        [] ->
            Ok (VList (List.reverse acc))

        x :: rest ->
            runDecoder applyValue globals dec x |> Result.andThen (\v -> decodeEach applyValue globals dec rest (v :: acc))


{-| Tries each decoder in turn (for `oneOf`), returning the first success or the last error. -}
tryDecoders : ApplyTo -> Globals -> List Value -> Value -> Result String Value
tryDecoders applyValue globals decs json =
    case decs of
        [] ->
            Err "oneOf: no decoder succeeded"

        d :: rest ->
            case runDecoder applyValue globals d json of
                Ok v ->
                    Ok v

                Err e ->
                    if List.isEmpty rest then
                        Err e

                    else
                        tryDecoders applyValue globals rest json


decodeAll : ApplyTo -> Globals -> List Value -> Value -> List Value -> Result String (List Value)
decodeAll applyValue globals decs json acc =
    case decs of
        [] ->
            Ok (List.reverse acc)

        d :: rest ->
            runDecoder applyValue globals d json |> Result.andThen (\v -> decodeAll applyValue globals rest json (v :: acc))


applyAll : ApplyTo -> Globals -> Value -> List Value -> Result String Value
applyAll applyValue globals f vals =
    case vals of
        [] ->
            Ok f

        v :: rest ->
            applyValue globals f v |> Result.andThen (\f2 -> applyAll applyValue globals f2 rest)


{-| `Json.Encode.object`: a list of `( key, value )` tuples becomes a `VRecord`. -}
encodeObject : Value -> Value
encodeObject pairs =
    case pairs of
        VList items ->
            VRecord (List.filterMap pairToField items)

        _ ->
            pairs


pairToField : Value -> Maybe ( String, Value )
pairToField v =
    case v of
        VTup [ VStr k, val ] ->
            Just ( k, val )

        _ ->
            Nothing


{-| `Json.Encode.list f xs`: encode each element with `f`, collecting a `VList`. -}
encodeList : ApplyTo -> Globals -> Value -> Value -> Result String Value
encodeList applyValue globals f xs =
    case xs of
        VList items ->
            encodeEach applyValue globals f items []

        _ ->
            Err "Encode.list expects a list"


encodeEach : ApplyTo -> Globals -> Value -> List Value -> List Value -> Result String Value
encodeEach applyValue globals f items acc =
    case items of
        [] ->
            Ok (VList (List.reverse acc))

        x :: rest ->
            applyValue globals f x |> Result.andThen (\v -> encodeEach applyValue globals f rest (v :: acc))


{-| Serialises an encoded `Value` to a compact JSON string (`Json.Encode.encode`). -}
jsonEncode : Value -> String
jsonEncode v =
    case v of
        VStr s ->
            "\"" ++ jsonEscape s ++ "\""

        VBool b ->
            if b then
                "true"

            else
                "false"

        VNum n ->
            if n == toFloat (round n) then
                String.fromInt (round n)

            else
                String.fromFloat n

        VCtor "Null" [] ->
            "null"

        VList items ->
            "[" ++ String.join "," (List.map jsonEncode items) ++ "]"

        VRecord fields ->
            "{" ++ String.join "," (List.map (\( k, val ) -> "\"" ++ jsonEscape k ++ "\":" ++ jsonEncode val) fields) ++ "}"

        _ ->
            "null"


jsonEscape : String -> String
jsonEscape s =
    s |> String.replace "\\" "\\\\" |> String.replace "\"" "\\\""


{-| A small JSON parser producing an interpreted `Value` (object→VRecord, array→VList, …). -}
parseJson : String -> Result String Value
parseJson s =
    jsonValue (skipWs (String.toList s)) |> Result.map Tuple.first


jsonValue : List Char -> Result String ( Value, List Char )
jsonValue chars =
    case chars of
        '"' :: rest ->
            jsonString rest ""

        '{' :: rest ->
            jsonObject (skipWs rest) []

        '[' :: rest ->
            jsonArray (skipWs rest) []

        't' :: 'r' :: 'u' :: 'e' :: rest ->
            Ok ( VBool True, rest )

        'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest ->
            Ok ( VBool False, rest )

        'n' :: 'u' :: 'l' :: 'l' :: rest ->
            Ok ( VCtor "Null" [], rest )

        c :: _ ->
            if c == '-' || Char.isDigit c then
                jsonNumber chars ""

            else
                Err "unexpected character in JSON"

        [] ->
            Err "unexpected end of JSON"


jsonString : List Char -> String -> Result String ( Value, List Char )
jsonString chars acc =
    case chars of
        '"' :: rest ->
            Ok ( VStr acc, rest )

        '\\' :: c :: rest ->
            jsonString rest (acc ++ escape c)

        c :: rest ->
            jsonString rest (acc ++ String.fromChar c)

        [] ->
            Err "unterminated JSON string"


escape : Char -> String
escape c =
    case c of
        'n' ->
            "\n"

        't' ->
            "\t"

        'r' ->
            "\u{000D}"

        _ ->
            String.fromChar c


jsonNumber : List Char -> String -> Result String ( Value, List Char )
jsonNumber chars acc =
    case chars of
        c :: rest ->
            if Char.isDigit c || c == '-' || c == '+' || c == '.' || c == 'e' || c == 'E' then
                jsonNumber rest (acc ++ String.fromChar c)

            else
                finishNumber acc chars

        [] ->
            finishNumber acc []


finishNumber : String -> List Char -> Result String ( Value, List Char )
finishNumber acc rest =
    case String.toFloat acc of
        Just n ->
            Ok ( VNum n, rest )

        Nothing ->
            Err ("bad JSON number: " ++ acc)


jsonObject : List Char -> List ( String, Value ) -> Result String ( Value, List Char )
jsonObject chars acc =
    case chars of
        '}' :: rest ->
            Ok ( VRecord (List.reverse acc), rest )

        '"' :: rest ->
            jsonString rest ""
                |> Result.andThen
                    (\( key, afterKey ) ->
                        case skipWs afterKey of
                            ':' :: afterColon ->
                                jsonValue (skipWs afterColon)
                                    |> Result.andThen
                                        (\( v, afterVal ) ->
                                            let
                                                pair =
                                                    ( valueToKey key, v )
                                            in
                                            case skipWs afterVal of
                                                ',' :: more ->
                                                    jsonObject (skipWs more) (pair :: acc)

                                                '}' :: more ->
                                                    Ok ( VRecord (List.reverse (pair :: acc)), more )

                                                _ ->
                                                    Err "expected ',' or '}' in object"
                                        )

                            _ ->
                                Err "expected ':' in object"
                    )

        _ ->
            Err "expected a key or '}' in object"


valueToKey : Value -> String
valueToKey v =
    case v of
        VStr s ->
            s

        _ ->
            ""


jsonArray : List Char -> List Value -> Result String ( Value, List Char )
jsonArray chars acc =
    case chars of
        ']' :: rest ->
            Ok ( VList (List.reverse acc), rest )

        _ ->
            jsonValue chars
                |> Result.andThen
                    (\( v, afterVal ) ->
                        case skipWs afterVal of
                            ',' :: more ->
                                jsonArray (skipWs more) (v :: acc)

                            ']' :: more ->
                                Ok ( VList (List.reverse (v :: acc)), more )

                            _ ->
                                Err "expected ',' or ']' in array"
                    )


skipWs : List Char -> List Char
skipWs chars =
    case chars of
        c :: rest ->
            if c == ' ' || c == '\n' || c == '\t' || c == '\u{000D}' then
                skipWs rest

            else
                chars

        [] ->
            []
