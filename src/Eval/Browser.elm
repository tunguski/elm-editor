module Eval.Browser exposing (processor)

{-| The interpreter's `Browser.*` builtins, as an {@link Eval.Core.Processor}. The editor drives
init/update/view itself, so evaluating `main = Browser.sandbox/element config` just yields the
config record. -}

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
    [ "Browser.sandbox", "Browser.element" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "Browser.sandbox", "Browser.element" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "Browser.sandbox", [ config ] ) ->
            Just (Ok config)

        ( "Browser.element", [ config ] ) ->
            Just (Ok config)

        _ ->
            Nothing
