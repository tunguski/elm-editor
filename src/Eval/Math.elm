module Eval.Math exposing (processor)

{-| The interpreter's unqualified numeric builtins (`Basics`' trig/rounding/etc.), as an
{@link Eval.Core.Processor}. All pure. -}

import Eval.Core exposing (Core, Processor)
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


names : List String
names =
    [ "cos", "sin", "tan", "sqrt", "toFloat", "round", "floor", "ceiling", "truncate", "abs" ]
        ++ [ "asin", "acos", "atan", "atan2", "logBase", "radians", "turns", "isNaN", "isInfinite" ]


arities : List ( Int, List String )
arities =
    -- atan2 and logBase take two numbers (arity 2 = the default); the rest take one.
    [ ( 1, [ "cos", "sin", "tan", "sqrt", "toFloat", "round", "floor", "ceiling", "truncate", "abs", "asin", "acos", "atan", "radians", "turns", "isNaN", "isInfinite" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "cos", [ VNum n ] ) ->
            Just (Ok (VNum (cos n)))

        ( "sin", [ VNum n ] ) ->
            Just (Ok (VNum (sin n)))

        ( "tan", [ VNum n ] ) ->
            Just (Ok (VNum (tan n)))

        ( "sqrt", [ VNum n ] ) ->
            Just (Ok (VNum (sqrt n)))

        ( "toFloat", [ VNum n ] ) ->
            Just (Ok (VNum n))

        ( "round", [ VNum n ] ) ->
            Just (Ok (VNum (toFloat (round n))))

        ( "floor", [ VNum n ] ) ->
            Just (Ok (VNum (toFloat (floor n))))

        ( "ceiling", [ VNum n ] ) ->
            Just (Ok (VNum (toFloat (ceiling n))))

        ( "truncate", [ VNum n ] ) ->
            Just (Ok (VNum (toFloat (truncate n))))

        ( "abs", [ VNum n ] ) ->
            Just (Ok (VNum (abs n)))

        ( "asin", [ VNum n ] ) ->
            Just (Ok (VNum (asin n)))

        ( "acos", [ VNum n ] ) ->
            Just (Ok (VNum (acos n)))

        ( "atan", [ VNum n ] ) ->
            Just (Ok (VNum (atan n)))

        ( "atan2", [ VNum y, VNum x ] ) ->
            Just (Ok (VNum (atan2 y x)))

        ( "logBase", [ VNum b, VNum n ] ) ->
            Just (Ok (VNum (logBase b n)))

        ( "radians", [ VNum n ] ) ->
            Just (Ok (VNum n))

        ( "turns", [ VNum n ] ) ->
            Just (Ok (VNum (2 * pi * n)))

        ( "isNaN", [ VNum n ] ) ->
            Just (Ok (VBool (isNaN n)))

        ( "isInfinite", [ VNum n ] ) ->
            Just (Ok (VBool (isInfinite n)))

        _ ->
            Nothing
