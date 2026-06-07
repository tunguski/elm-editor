module Eval.Random exposing (processor)

{-| The interpreter's `Random.*` builtins, as an {@link Eval.Core.Processor}. A generator is a tagged
`VCtor "Random.Gen" [...]` value the editor samples with its seed; `Random.generate` is a command. -}

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
    [ "Random.int", "Random.float", "Random.uniform", "Random.constant", "Random.map", "Random.map2", "Random.map3", "Random.pair", "Random.list", "Random.andThen", "Random.generate" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "Random.constant" ] ), ( 3, [ "Random.map2" ] ), ( 4, [ "Random.map3" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "Random.int", [ VNum lo, VNum hi ] ) ->
            Just (Ok (VCtor "Random.Gen" [ VStr "int", VNum lo, VNum hi ]))

        ( "Random.float", [ VNum lo, VNum hi ] ) ->
            Just (Ok (VCtor "Random.Gen" [ VStr "float", VNum lo, VNum hi ]))

        ( "Random.uniform", [ first, VList rest ] ) ->
            Just (Ok (VCtor "Random.Gen" [ VStr "uniform", VList (first :: rest) ]))

        ( "Random.constant", [ x ] ) ->
            Just (Ok (VCtor "Random.Gen" [ VStr "constant", x ]))

        ( "Random.map", [ f, g ] ) ->
            Just (Ok (VCtor "Random.Gen" [ VStr "map", f, g ]))

        ( "Random.map2", [ f, g1, g2 ] ) ->
            Just (Ok (VCtor "Random.Gen" [ VStr "map2", f, g1, g2 ]))

        ( "Random.map3", [ f, g1, g2, g3 ] ) ->
            Just (Ok (VCtor "Random.Gen" [ VStr "map3", f, g1, g2, g3 ]))

        ( "Random.pair", [ g1, g2 ] ) ->
            Just (Ok (VCtor "Random.Gen" [ VStr "pair", g1, g2 ]))

        ( "Random.list", [ VNum n, g ] ) ->
            Just (Ok (VCtor "Random.Gen" [ VStr "list", VNum n, g ]))

        ( "Random.andThen", [ f, g ] ) ->
            Just (Ok (VCtor "Random.Gen" [ VStr "andThen", f, g ]))

        ( "Random.generate", [ toMsg, gen ] ) ->
            Just (Ok (VCtor "Cmd.random" [ toMsg, gen ]))

        _ ->
            Nothing
