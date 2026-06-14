module Eval.Encode exposing (processor)

{-| The interpreter's `Json.Encode.*` builtins, as an {@link Eval.Core.Processor}. JSON values share
the interpreter's value representation, so the scalar encoders are identities; objects/lists/encode
delegate to the shared JSON layer in `Eval.Json`. -}

import Eval.Core exposing (Core, Processor)
import Eval.Json
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


names : List String
names =
    [ "Encode.int", "Encode.float", "Encode.string", "Encode.bool", "Encode.object", "Encode.list", "Encode.array", "Encode.set", "Encode.dict", "Encode.encode" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "Encode.int", "Encode.float", "Encode.string", "Encode.bool", "Encode.object" ] )
    , ( 2, [ "Encode.set" ] )
    , ( 3, [ "Encode.dict" ] )
    ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run core globals name args =
    case ( name, args ) of
        ( "Encode.int", [ v ] ) ->
            Just (Ok v)

        ( "Encode.float", [ v ] ) ->
            Just (Ok v)

        ( "Encode.string", [ v ] ) ->
            Just (Ok v)

        ( "Encode.bool", [ v ] ) ->
            Just (Ok v)

        ( "Encode.object", [ pairs ] ) ->
            Just (Ok (Eval.Json.encodeObject pairs))

        ( "Encode.list", [ f, xs ] ) ->
            Just (Eval.Json.encodeList core.apply globals f xs)

        ( "Encode.array", [ f, arr ] ) ->
            Just (Eval.Json.encodeArray core.apply globals f arr)

        ( "Encode.set", [ f, set ] ) ->
            Just (Eval.Json.encodeSet core.apply globals f set)

        ( "Encode.dict", [ toKey, toVal, dict ] ) ->
            Just (Eval.Json.encodeDict core.apply globals toKey toVal dict)

        ( "Encode.encode", [ _, value ] ) ->
            Just (Ok (VStr (Eval.Json.jsonEncode value)))

        _ ->
            Nothing
