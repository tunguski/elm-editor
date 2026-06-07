module Eval.String exposing (processor)

{-| The interpreter's `String.*` builtins, as a self-contained {@link Eval.Core.Processor}. `Eval`
imports just `processor`, hands it the shared `Core`, and folds the result into its builtin tables and
dispatch — so all the `String` logic lives here rather than scattered through `Eval`'s big `case`.
-}

import Eval.Core exposing (Core, Processor, charOf, maybeValue)
import Eval.Render exposing (renderStr)
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


{-| Every `String` builtin this module handles (folded into `Eval.builtinNames`). -}
names : List String
names =
    [ "String.fromInt", "String.fromFloat", "String.reverse", "String.length", "String.toUpper", "String.toLower", "String.trim", "String.trimLeft", "String.trimRight", "String.concat", "String.words", "String.isEmpty" ]
        ++ [ "String.toInt", "String.toFloat", "String.fromChar", "String.toList", "String.fromList", "String.cons", "String.uncons", "String.lines", "String.join", "String.append" ]
        ++ [ "String.contains", "String.startsWith", "String.endsWith", "String.left", "String.right", "String.dropLeft", "String.dropRight", "String.repeat", "String.slice", "String.split" ]
        ++ [ "String.indexes", "String.indices", "String.map", "String.filter", "String.any", "String.all", "String.foldl", "String.foldr", "String.padLeft", "String.padRight", "String.pad", "String.replace" ]


{-| Non-default arities (the interpreter defaults an unlisted builtin to arity 2). -}
arities : List ( Int, List String )
arities =
    [ ( 1, [ "String.fromInt", "String.fromFloat", "String.reverse", "String.length", "String.toUpper", "String.toLower", "String.trim", "String.trimLeft", "String.trimRight", "String.concat", "String.words", "String.isEmpty", "String.toInt", "String.toFloat", "String.fromChar", "String.toList", "String.fromList", "String.uncons", "String.lines" ] )
    , ( 3, [ "String.slice", "String.foldl", "String.foldr", "String.padLeft", "String.padRight", "String.pad", "String.replace" ] )
    ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run core globals name args =
    case ( name, args ) of
        ( "String.fromInt", [ VNum n ] ) ->
            Just (Ok (VStr (String.fromInt (round n))))

        ( "String.fromFloat", [ VNum n ] ) ->
            Just (Ok (VStr (String.fromFloat n)))

        ( "String.reverse", [ VStr s ] ) ->
            Just (Ok (VStr (String.reverse s)))

        ( "String.length", [ VStr s ] ) ->
            Just (Ok (VNum (toFloat (String.length s))))

        ( "String.toUpper", [ VStr s ] ) ->
            Just (Ok (VStr (String.toUpper s)))

        ( "String.toLower", [ VStr s ] ) ->
            Just (Ok (VStr (String.toLower s)))

        ( "String.trim", [ VStr s ] ) ->
            Just (Ok (VStr (String.trim s)))

        ( "String.trimLeft", [ VStr s ] ) ->
            Just (Ok (VStr (String.trimLeft s)))

        ( "String.trimRight", [ VStr s ] ) ->
            Just (Ok (VStr (String.trimRight s)))

        ( "String.concat", [ VList xs ] ) ->
            Just (Ok (VStr (String.concat (List.map renderStr xs))))

        ( "String.words", [ VStr s ] ) ->
            Just (Ok (VList (List.map VStr (String.words s))))

        ( "String.isEmpty", [ VStr s ] ) ->
            Just (Ok (VBool (String.isEmpty s)))

        ( "String.join", [ VStr sep, VList xs ] ) ->
            Just (Ok (VStr (String.join sep (List.map renderStr xs))))

        ( "String.append", [ VStr a, VStr b ] ) ->
            Just (Ok (VStr (a ++ b)))

        ( "String.contains", [ VStr sub, VStr s ] ) ->
            Just (Ok (VBool (String.contains sub s)))

        ( "String.startsWith", [ VStr pre, VStr s ] ) ->
            Just (Ok (VBool (String.startsWith pre s)))

        ( "String.endsWith", [ VStr suf, VStr s ] ) ->
            Just (Ok (VBool (String.endsWith suf s)))

        ( "String.left", [ VNum n, VStr s ] ) ->
            Just (Ok (VStr (String.left (round n) s)))

        ( "String.right", [ VNum n, VStr s ] ) ->
            Just (Ok (VStr (String.right (round n) s)))

        ( "String.dropLeft", [ VNum n, VStr s ] ) ->
            Just (Ok (VStr (String.dropLeft (round n) s)))

        ( "String.dropRight", [ VNum n, VStr s ] ) ->
            Just (Ok (VStr (String.dropRight (round n) s)))

        ( "String.repeat", [ VNum n, VStr s ] ) ->
            Just (Ok (VStr (String.repeat (round n) s)))

        ( "String.slice", [ VNum a, VNum b, VStr s ] ) ->
            Just (Ok (VStr (String.slice (round a) (round b) s)))

        ( "String.split", [ VStr sep, VStr s ] ) ->
            Just (Ok (VList (List.map VStr (String.split sep s))))

        ( "String.indexes", [ VStr sub, VStr s ] ) ->
            Just (Ok (VList (List.map (\i -> VNum (toFloat i)) (String.indexes sub s))))

        ( "String.indices", [ VStr sub, VStr s ] ) ->
            Just (Ok (VList (List.map (\i -> VNum (toFloat i)) (String.indexes sub s))))

        ( "String.fromChar", [ VChar c ] ) ->
            Just (Ok (VStr (String.fromChar c)))

        ( "String.toList", [ VStr s ] ) ->
            Just (Ok (VList (List.map VChar (String.toList s))))

        ( "String.fromList", [ VList xs ] ) ->
            Just (Ok (VStr (String.fromList (List.filterMap charOf xs))))

        ( "String.cons", [ VChar c, VStr s ] ) ->
            Just (Ok (VStr (String.cons c s)))

        ( "String.uncons", [ VStr s ] ) ->
            Just (Ok (maybeValue (Maybe.map (\( c, rest ) -> VTup [ VChar c, VStr rest ]) (String.uncons s))))

        ( "String.toInt", [ VStr s ] ) ->
            Just (Ok (maybeValue (Maybe.map (\n -> VNum (toFloat n)) (String.toInt (String.trim s)))))

        ( "String.toFloat", [ VStr s ] ) ->
            Just (Ok (maybeValue (Maybe.map VNum (String.toFloat (String.trim s)))))

        ( "String.lines", [ VStr s ] ) ->
            Just (Ok (VList (List.map VStr (String.lines s))))

        ( "String.replace", [ VStr from, VStr to, VStr s ] ) ->
            Just (Ok (VStr (String.replace from to s)))

        ( "String.padLeft", [ VNum n, VChar c, VStr s ] ) ->
            Just (Ok (VStr (String.padLeft (round n) c s)))

        ( "String.padRight", [ VNum n, VChar c, VStr s ] ) ->
            Just (Ok (VStr (String.padRight (round n) c s)))

        ( "String.pad", [ VNum n, VChar c, VStr s ] ) ->
            -- Pad both sides to width n; the extra character goes on the right when odd.
            let
                total =
                    Basics.max 0 (round n - String.length s)

                left =
                    total // 2
            in
            Just (Ok (VStr (String.repeat left (String.fromChar c) ++ s ++ String.repeat (total - left) (String.fromChar c))))

        ( "String.map", [ f, VStr s ] ) ->
            Just
                (core.mapValues globals f (List.map VChar (String.toList s))
                    |> Result.map (\ys -> VStr (String.fromList (List.filterMap charOf ys)))
                )

        ( "String.filter", [ f, VStr s ] ) ->
            Just
                (core.filterValues globals f (List.map VChar (String.toList s))
                    |> Result.map (\ys -> VStr (String.fromList (List.filterMap charOf ys)))
                )

        ( "String.any", [ f, VStr s ] ) ->
            Just
                (core.mapValues globals f (List.map VChar (String.toList s))
                    |> Result.map (\bs -> VBool (List.any (\b -> b == VBool True) bs))
                )

        ( "String.all", [ f, VStr s ] ) ->
            Just
                (core.mapValues globals f (List.map VChar (String.toList s))
                    |> Result.map (\bs -> VBool (List.all (\b -> b == VBool True) bs))
                )

        ( "String.foldl", [ f, acc, VStr s ] ) ->
            Just (core.foldlValues globals f acc (List.map VChar (String.toList s)))

        ( "String.foldr", [ f, acc, VStr s ] ) ->
            Just (core.foldlValues globals f acc (List.reverse (List.map VChar (String.toList s))))

        _ ->
            Nothing
