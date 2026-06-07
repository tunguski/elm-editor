module EvalEncode exposing (processor)

{-| The interpreter's `Json.Encode.*` builtins, as an {@link EvalCore.Processor}. JSON values share
the interpreter's value representation, so the scalar encoders are identities; objects/lists/encode
delegate to the shared JSON layer in `EvalJson`. -}

import EvalCore exposing (Core, Processor)
import EvalJson
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


names : List String
names =
    [ "Encode.int", "Encode.float", "Encode.string", "Encode.bool", "Encode.object", "Encode.list", "Encode.encode" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "Encode.int", "Encode.float", "Encode.string", "Encode.bool", "Encode.object" ] ) ]


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
            Just (Ok (EvalJson.encodeObject pairs))

        ( "Encode.list", [ f, xs ] ) ->
            Just (EvalJson.encodeList core.apply globals f xs)

        ( "Encode.encode", [ _, value ] ) ->
            Just (Ok (VStr (EvalJson.jsonEncode value)))

        _ ->
            Nothing
