module Eval.Bitwise exposing (processor)

{-| The interpreter's `Bitwise.*` builtins, as an {@link Eval.Core.Processor}. All pure (no `Core`). -}

import Bitwise
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
    [ "Bitwise.and", "Bitwise.or", "Bitwise.xor", "Bitwise.complement", "Bitwise.shiftLeftBy", "Bitwise.shiftRightBy", "Bitwise.shiftRightZfBy" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "Bitwise.complement" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "Bitwise.and", [ VNum a, VNum b ] ) ->
            Just (Ok (VNum (toFloat (Bitwise.and (truncate a) (truncate b)))))

        ( "Bitwise.or", [ VNum a, VNum b ] ) ->
            Just (Ok (VNum (toFloat (Bitwise.or (truncate a) (truncate b)))))

        ( "Bitwise.xor", [ VNum a, VNum b ] ) ->
            Just (Ok (VNum (toFloat (Bitwise.xor (truncate a) (truncate b)))))

        ( "Bitwise.complement", [ VNum a ] ) ->
            Just (Ok (VNum (toFloat (Bitwise.complement (truncate a)))))

        ( "Bitwise.shiftLeftBy", [ VNum n, VNum a ] ) ->
            Just (Ok (VNum (toFloat (Bitwise.shiftLeftBy (truncate n) (truncate a)))))

        ( "Bitwise.shiftRightBy", [ VNum n, VNum a ] ) ->
            Just (Ok (VNum (toFloat (Bitwise.shiftRightBy (truncate n) (truncate a)))))

        ( "Bitwise.shiftRightZfBy", [ VNum n, VNum a ] ) ->
            Just (Ok (VNum (toFloat (Bitwise.shiftRightZfBy (truncate n) (truncate a)))))

        _ ->
            Nothing
