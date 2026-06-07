module Eval.Basics exposing (processor)

{-| The interpreter's unqualified `Basics` builtins (`toString`/`negate`/`identity`/`min`/`clamp`/
`compare`/…), as an {@link Eval.Core.Processor}. -}

import Eval.Core exposing (Core, Processor, valueCompare)
import Eval.Render exposing (renderValue)
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


names : List String
names =
    [ "toString", "negate", "not", "identity", "always", "min", "max", "modBy", "remainderBy", "clamp", "xor", "compare" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "toString", "negate", "not", "identity" ] ), ( 3, [ "clamp" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "toString", [ VStr s ] ) ->
            Just (Ok (VStr s))

        ( "toString", [ v ] ) ->
            Just (Ok (VStr (renderValue v)))

        ( "negate", [ VNum n ] ) ->
            Just (Ok (VNum (negate n)))

        ( "not", [ VBool b ] ) ->
            Just (Ok (VBool (not b)))

        ( "xor", [ VBool a, VBool b ] ) ->
            Just (Ok (VBool (xor a b)))

        ( "identity", [ v ] ) ->
            Just (Ok v)

        ( "always", [ v, _ ] ) ->
            Just (Ok v)

        ( "min", [ VNum a, VNum b ] ) ->
            Just (Ok (VNum (Basics.min a b)))

        ( "max", [ VNum a, VNum b ] ) ->
            Just (Ok (VNum (Basics.max a b)))

        ( "clamp", [ VNum lo, VNum hi, VNum x ] ) ->
            Just (Ok (VNum (Basics.clamp lo hi x)))

        ( "modBy", [ VNum m, VNum n ] ) ->
            Just (divBy "modBy" modBy m n)

        ( "remainderBy", [ VNum m, VNum n ] ) ->
            Just (divBy "remainderBy" remainderBy m n)

        ( "compare", [ a, b ] ) ->
            Just (Ok (orderValue (valueCompare a b)))

        _ ->
            Nothing


{-| `modBy`/`remainderBy`: apply the integer op, erroring on a zero divisor. -}
divBy : String -> (Int -> Int -> Int) -> Float -> Float -> Result String Value
divBy name op m n =
    if round m == 0 then
        Err (name ++ ": division by zero")

    else
        Ok (VNum (toFloat (op (round m) (round n))))


{-| An `Order` as the interpreter's `LT`/`EQ`/`GT` value. -}
orderValue : Order -> Value
orderValue o =
    case o of
        LT ->
            VCtor "LT" []

        EQ ->
            VCtor "EQ" []

        GT ->
            VCtor "GT" []
