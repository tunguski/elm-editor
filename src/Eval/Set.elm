module Eval.Set exposing (processor)

{-| The interpreter's `Set.*` builtins, as an {@link Eval.Core.Processor}. A set is `VCtor "Set"
[ VList sortedElems ]`; the membership/insert helpers are kept private here. -}

import Eval.Core exposing (Core, Processor, valueCompare, valueEq)
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


names : List String
names =
    [ "Set.empty", "Set.singleton", "Set.fromList", "Set.toList", "Set.insert", "Set.remove", "Set.member", "Set.size", "Set.isEmpty", "Set.union", "Set.diff", "Set.intersect", "Set.foldl", "Set.foldr", "Set.map", "Set.filter", "Set.partition" ]


arities : List ( Int, List String )
arities =
    [ ( 0, [ "Set.empty" ] )
    , ( 1, [ "Set.singleton", "Set.fromList", "Set.toList", "Set.size", "Set.isEmpty" ] )
    , ( 3, [ "Set.foldl", "Set.foldr" ] )
    ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run core globals name args =
    case ( name, args ) of
        ( "Set.empty", [] ) ->
            Just (Ok (mkSet []))

        ( "Set.singleton", [ x ] ) ->
            Just (Ok (mkSet [ x ]))

        ( "Set.fromList", [ VList xs ] ) ->
            Just (Ok (mkSet (List.foldl setInsert [] xs)))

        ( "Set.toList", [ s ] ) ->
            Just (Ok (VList (setElems s)))

        ( "Set.size", [ s ] ) ->
            Just (Ok (VNum (toFloat (List.length (setElems s)))))

        ( "Set.isEmpty", [ s ] ) ->
            Just (Ok (VBool (List.isEmpty (setElems s))))

        ( "Set.member", [ x, s ] ) ->
            Just (Ok (VBool (List.any (valueEq x) (setElems s))))

        ( "Set.insert", [ x, s ] ) ->
            Just (Ok (mkSet (setInsert x (setElems s))))

        ( "Set.remove", [ x, s ] ) ->
            Just (Ok (mkSet (List.filter (\y -> not (valueEq x y)) (setElems s))))

        ( "Set.union", [ a, b ] ) ->
            Just (Ok (mkSet (List.foldl setInsert (setElems a) (setElems b))))

        ( "Set.intersect", [ a, b ] ) ->
            Just (Ok (mkSet (List.filter (\x -> List.any (valueEq x) (setElems b)) (setElems a))))

        ( "Set.diff", [ a, b ] ) ->
            Just (Ok (mkSet (List.filter (\x -> not (List.any (valueEq x) (setElems b))) (setElems a))))

        ( "Set.foldl", [ f, acc, s ] ) ->
            Just (core.foldlValues globals f acc (setElems s))

        ( "Set.foldr", [ f, acc, s ] ) ->
            Just (core.foldlValues globals f acc (List.reverse (setElems s)))

        ( "Set.map", [ f, s ] ) ->
            Just (core.mapValues globals f (setElems s) |> Result.map (\ys -> mkSet (List.foldl setInsert [] ys)))

        ( "Set.filter", [ f, s ] ) ->
            Just (core.filterValues globals f (setElems s) |> Result.map mkSet)

        ( "Set.partition", [ f, s ] ) ->
            Just
                (core.filterValues globals f (setElems s)
                    |> Result.map
                        (\yes ->
                            VTup
                                [ mkSet yes
                                , mkSet (List.filter (\x -> not (List.any (valueEq x) yes)) (setElems s))
                                ]
                        )
                )

        _ ->
            Nothing


mkSet : List Value -> Value
mkSet elems =
    VCtor "Set" [ VList (List.sortWith valueCompare elems) ]


setElems : Value -> List Value
setElems v =
    case v of
        VCtor "Set" [ VList xs ] ->
            xs

        _ ->
            []


setInsert : Value -> List Value -> List Value
setInsert x xs =
    if List.any (valueEq x) xs then
        xs

    else
        xs ++ [ x ]
