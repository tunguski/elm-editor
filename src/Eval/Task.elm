module Eval.Task exposing (processor)

{-| The interpreter's `Task.*` builtins, as an {@link Eval.Core.Processor}. `Task.perform`/`attempt`
become commands the editor resolves. -}

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
    [ "Task.perform", "Task.attempt" ]


arities : List ( Int, List String )
arities =
    []


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "Task.perform", [ toMsg, task ] ) ->
            Just (Ok (VCtor "Cmd.task" [ toMsg, task ]))

        ( "Task.attempt", [ toMsg, task ] ) ->
            Just (Ok (VCtor "Cmd.taskAttempt" [ toMsg, task ]))

        _ ->
            Nothing
