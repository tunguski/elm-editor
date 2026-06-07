module Eval.Time exposing (processor)

{-| The interpreter's `Time.*` builtins, as an {@link Eval.Core.Processor}. A Posix time is just its
millisecond `VNum`; `Time.every` is a subscription the editor drives as a live tick. -}

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
    [ "Time.millisToPosix", "Time.posixToMillis", "Time.toHour", "Time.toMinute", "Time.toSecond", "Time.every" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "Time.millisToPosix", "Time.posixToMillis" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "Time.millisToPosix", [ VNum n ] ) ->
            Just (Ok (VNum n))

        ( "Time.posixToMillis", [ VNum n ] ) ->
            Just (Ok (VNum n))

        ( "Time.toHour", [ _, VNum ms ] ) ->
            Just (Ok (VNum (toFloat (modBy 24 (round ms // 3600000)))))

        ( "Time.toMinute", [ _, VNum ms ] ) ->
            Just (Ok (VNum (toFloat (modBy 60 (round ms // 60000)))))

        ( "Time.toSecond", [ _, VNum ms ] ) ->
            Just (Ok (VNum (toFloat (modBy 60 (round ms // 1000)))))

        ( "Time.every", [ VNum interval, toMsg ] ) ->
            Just (Ok (VCtor "Sub.every" [ VNum interval, toMsg ]))

        _ ->
            Nothing
