module Eval.Debug exposing (processor)

{-| The interpreter's `Debug.*` builtins, as an {@link Eval.Core.Processor}. -}

import Eval.Core exposing (Core, Processor)
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
    [ "Debug.toString", "Debug.log", "Debug.todo" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "Debug.toString", "Debug.todo" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "Debug.toString", [ v ] ) ->
            Just (Ok (VStr (renderValue v)))

        ( "Debug.log", [ _, v ] ) ->
            Just (Ok v)

        ( "Debug.todo", [ VStr msg ] ) ->
            Just (Err ("TODO: " ++ msg))

        _ ->
            Nothing
