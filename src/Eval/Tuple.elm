module Eval.Tuple exposing (processor)

{-| The interpreter's `Tuple.*` builtins, as an {@link Eval.Core.Processor}. -}

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
    [ "Tuple.first", "Tuple.second", "Tuple.pair", "Tuple.mapFirst", "Tuple.mapSecond", "Tuple.mapBoth" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "Tuple.first", "Tuple.second" ] ), ( 3, [ "Tuple.mapBoth" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run core globals name args =
    case ( name, args ) of
        ( "Tuple.first", [ VTup (a :: _) ] ) ->
            Just (Ok a)

        ( "Tuple.second", [ VTup (_ :: b :: _) ] ) ->
            Just (Ok b)

        ( "Tuple.pair", [ a, b ] ) ->
            Just (Ok (VTup [ a, b ]))

        ( "Tuple.mapFirst", [ f, VTup [ a, b ] ] ) ->
            Just (core.apply globals f a |> Result.map (\x -> VTup [ x, b ]))

        ( "Tuple.mapSecond", [ f, VTup [ a, b ] ] ) ->
            Just (core.apply globals f b |> Result.map (\y -> VTup [ a, y ]))

        ( "Tuple.mapBoth", [ f, g, VTup [ a, b ] ] ) ->
            Just
                (core.apply globals f a
                    |> Result.andThen (\x -> core.apply globals g b |> Result.map (\y -> VTup [ x, y ]))
                )

        _ ->
            Nothing
