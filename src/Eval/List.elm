module Eval.List exposing (processor)

{-| The interpreter's `List.*` builtins, as an {@link Eval.Core.Processor}. The higher-order ones use
the shared combinators in `Core` (`mapValues`/`filterValues`/`foldlValues`) plus a few List-specific
fold helpers kept private here. -}

import Eval.Core exposing (Core, Processor, asList, asNum, keepJust, maybeValue, pairKey, pairValue, valueCompare, valueEq)
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


names : List String
names =
    [ "List.range", "List.map", "List.length", "List.sum", "List.reverse", "List.head", "List.tail", "List.isEmpty", "List.maximum", "List.minimum", "List.sort", "List.sortBy", "List.concat", "List.product" ]
        ++ [ "List.append", "List.member", "List.filter", "List.filterMap", "List.concatMap", "List.take", "List.drop", "List.repeat", "List.singleton", "List.any", "List.all", "List.indexedMap", "List.map2", "List.foldl", "List.foldr" ]
        ++ [ "List.intersperse", "List.partition", "List.unzip", "List.map3", "List.map4", "List.map5", "List.sortWith" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "List.length", "List.sum", "List.reverse", "List.head", "List.tail", "List.isEmpty", "List.maximum", "List.minimum", "List.sort", "List.concat", "List.product", "List.singleton", "List.unzip" ] )
    , ( 3, [ "List.foldl", "List.foldr", "List.map2" ] )
    , ( 4, [ "List.map3" ] )
    , ( 5, [ "List.map4" ] )
    , ( 6, [ "List.map5" ] )
    ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run core globals name args =
    case ( name, args ) of
        ( "List.length", [ VList xs ] ) ->
            Just (Ok (VNum (toFloat (List.length xs))))

        ( "List.sum", [ VList xs ] ) ->
            Just (Ok (VNum (List.sum (List.filterMap asNum xs))))

        ( "List.range", [ VNum a, VNum b ] ) ->
            Just (Ok (VList (List.map (\n -> VNum (toFloat n)) (List.range (round a) (round b)))))

        ( "List.map", [ f, VList xs ] ) ->
            Just (core.mapValues globals f xs |> Result.map VList)

        ( "List.reverse", [ VList xs ] ) ->
            Just (Ok (VList (List.reverse xs)))

        ( "List.head", [ VList xs ] ) ->
            Just (Ok (maybeValue (List.head xs)))

        ( "List.tail", [ VList xs ] ) ->
            Just (Ok (maybeValue (Maybe.map VList (List.tail xs))))

        ( "List.isEmpty", [ VList xs ] ) ->
            Just (Ok (VBool (List.isEmpty xs)))

        ( "List.maximum", [ VList xs ] ) ->
            Just (Ok (maybeValue (Maybe.map VNum (List.maximum (List.filterMap asNum xs)))))

        ( "List.minimum", [ VList xs ] ) ->
            Just (Ok (maybeValue (Maybe.map VNum (List.minimum (List.filterMap asNum xs)))))

        ( "List.sort", [ VList xs ] ) ->
            Just (Ok (VList (List.sortWith valueCompare xs)))

        ( "List.sortBy", [ f, VList xs ] ) ->
            -- Keys are computed once per element, then compared (a Schwartzian transform).
            Just
                (keyValues core globals f xs
                    |> Result.map (\keyed -> VList (List.map Tuple.second (List.sortWith (\a b -> valueCompare (Tuple.first a) (Tuple.first b)) keyed)))
                )

        ( "List.concat", [ VList xs ] ) ->
            Just (Ok (VList (List.concatMap asList xs)))

        ( "List.product", [ VList xs ] ) ->
            Just (Ok (VNum (List.product (List.filterMap asNum xs))))

        ( "List.append", [ VList a, VList b ] ) ->
            Just (Ok (VList (a ++ b)))

        ( "List.member", [ x, VList xs ] ) ->
            Just (Ok (VBool (List.any (valueEq x) xs)))

        ( "List.filter", [ f, VList xs ] ) ->
            Just (core.filterValues globals f xs |> Result.map VList)

        ( "List.filterMap", [ f, VList xs ] ) ->
            Just (core.mapValues globals f xs |> Result.map (\ys -> VList (List.filterMap keepJust ys)))

        ( "List.concatMap", [ f, VList xs ] ) ->
            Just (core.mapValues globals f xs |> Result.map (\ys -> VList (List.concatMap asList ys)))

        ( "List.take", [ VNum n, VList xs ] ) ->
            Just (Ok (VList (List.take (round n) xs)))

        ( "List.drop", [ VNum n, VList xs ] ) ->
            Just (Ok (VList (List.drop (round n) xs)))

        ( "List.repeat", [ VNum n, x ] ) ->
            Just (Ok (VList (List.repeat (round n) x)))

        ( "List.singleton", [ x ] ) ->
            Just (Ok (VList [ x ]))

        ( "List.any", [ f, VList xs ] ) ->
            Just (anyValues core globals f xs |> Result.map VBool)

        ( "List.all", [ f, VList xs ] ) ->
            Just (allValues core globals f xs |> Result.map VBool)

        ( "List.indexedMap", [ f, VList xs ] ) ->
            Just (indexedMapValues core globals f 0 xs |> Result.map VList)

        ( "List.map2", [ f, VList a, VList b ] ) ->
            Just (map2Values core globals f a b |> Result.map VList)

        ( "List.foldl", [ f, acc, VList xs ] ) ->
            Just (core.foldlValues globals f acc xs)

        ( "List.foldr", [ f, acc, VList xs ] ) ->
            Just (core.foldlValues globals f acc (List.reverse xs))

        ( "List.intersperse", [ sep, VList xs ] ) ->
            Just (Ok (VList (List.intersperse sep xs)))

        ( "List.partition", [ f, VList xs ] ) ->
            Just (partitionValues core globals f xs)

        ( "List.unzip", [ VList xs ] ) ->
            Just (Ok (VTup [ VList (List.filterMap pairKey xs), VList (List.filterMap pairValue xs) ]))

        ( "List.map3", [ f, VList xs, VList ys, VList zs ] ) ->
            Just (map3Values core globals f xs ys zs |> Result.map VList)

        ( "List.map4", [ f, VList a, VList b, VList c, VList d ] ) ->
            Just (mapNValues core globals f [ a, b, c, d ] |> Result.map VList)

        ( "List.map5", [ f, VList a, VList b, VList c, VList d, VList e ] ) ->
            Just (mapNValues core globals f [ a, b, c, d, e ] |> Result.map VList)

        ( "List.sortWith", [ f, VList xs ] ) ->
            Just (Ok (VList (List.sortWith (orderFromCompare core globals f) xs)))

        _ ->
            Nothing



-- List-specific apply-dependent fold helpers (the generic map/filter/fold live in Core).


keyValues : Core -> Globals -> Value -> List Value -> Result String (List ( Value, Value ))
keyValues core globals f xs =
    case xs of
        [] ->
            Ok []

        x :: rest ->
            core.apply globals f x
                |> Result.andThen (\k -> keyValues core globals f rest |> Result.map (\ks -> ( k, x ) :: ks))


anyValues : Core -> Globals -> Value -> List Value -> Result String Bool
anyValues core globals f xs =
    case xs of
        [] ->
            Ok False

        x :: rest ->
            core.apply globals f x
                |> Result.andThen
                    (\b ->
                        if b == VBool True then
                            Ok True

                        else
                            anyValues core globals f rest
                    )


allValues : Core -> Globals -> Value -> List Value -> Result String Bool
allValues core globals f xs =
    case xs of
        [] ->
            Ok True

        x :: rest ->
            core.apply globals f x
                |> Result.andThen
                    (\b ->
                        if b == VBool True then
                            allValues core globals f rest

                        else
                            Ok False
                    )


indexedMapValues : Core -> Globals -> Value -> Int -> List Value -> Result String (List Value)
indexedMapValues core globals f i xs =
    case xs of
        [] ->
            Ok []

        x :: rest ->
            core.apply globals f (VNum (toFloat i))
                |> Result.andThen (\g -> core.apply globals g x)
                |> Result.andThen (\y -> indexedMapValues core globals f (i + 1) rest |> Result.map (\ys -> y :: ys))


map2Values : Core -> Globals -> Value -> List Value -> List Value -> Result String (List Value)
map2Values core globals f xs ys =
    case ( xs, ys ) of
        ( x :: xrest, y :: yrest ) ->
            core.apply globals f x
                |> Result.andThen (\g -> core.apply globals g y)
                |> Result.andThen (\z -> map2Values core globals f xrest yrest |> Result.map (\zs -> z :: zs))

        _ ->
            Ok []


map3Values : Core -> Globals -> Value -> List Value -> List Value -> List Value -> Result String (List Value)
map3Values core globals f xs ys zs =
    case ( xs, ys, zs ) of
        ( x :: xr, y :: yr, z :: zr ) ->
            core.apply globals f x
                |> Result.andThen (\g -> core.apply globals g y)
                |> Result.andThen (\h -> core.apply globals h z)
                |> Result.andThen (\v -> map3Values core globals f xr yr zr |> Result.map (\vs -> v :: vs))

        _ ->
            Ok []


mapNValues : Core -> Globals -> Value -> List (List Value) -> Result String (List Value)
mapNValues core globals f lists =
    if List.isEmpty lists || List.any List.isEmpty lists then
        Ok []

    else
        core.applyAll globals f (List.filterMap List.head lists)
            |> Result.andThen
                (\v ->
                    mapNValues core globals f (List.map (List.drop 1) lists)
                        |> Result.map (\vs -> v :: vs)
                )


partitionValues : Core -> Globals -> Value -> List Value -> Result String Value
partitionValues core globals f xs =
    case xs of
        [] ->
            Ok (VTup [ VList [], VList [] ])

        x :: rest ->
            core.apply globals f x
                |> Result.andThen
                    (\keep ->
                        partitionValues core globals f rest
                            |> Result.map
                                (\r ->
                                    case r of
                                        VTup [ VList yes, VList no ] ->
                                            if keep == VBool True then
                                                VTup [ VList (x :: yes), VList no ]

                                            else
                                                VTup [ VList yes, VList (x :: no) ]

                                        _ ->
                                            r
                                )
                    )


orderFromCompare : Core -> Globals -> Value -> Value -> Value -> Order
orderFromCompare core globals f a b =
    case core.apply globals f a |> Result.andThen (\g -> core.apply globals g b) of
        Ok (VCtor "LT" _) ->
            LT

        Ok (VCtor "GT" _) ->
            GT

        _ ->
            EQ
