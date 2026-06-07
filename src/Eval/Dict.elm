module Eval.Dict exposing (processor)

{-| The interpreter's `Dict.*` builtins, as an {@link Eval.Core.Processor}. A dict is `VCtor "Dict"
[ VList pairs ]` where each pair is a `(key, value)` tuple; the assoc-list helpers are private here. -}

import Eval.Core exposing (Core, Processor, maybeValue, pairKey, pairValue, valueEq)
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


names : List String
names =
    [ "Dict.empty", "Dict.singleton", "Dict.fromList", "Dict.toList", "Dict.keys", "Dict.values", "Dict.size", "Dict.isEmpty", "Dict.member", "Dict.get" ]
        ++ [ "Dict.insert", "Dict.remove", "Dict.map", "Dict.filter", "Dict.foldl", "Dict.foldr", "Dict.partition", "Dict.union", "Dict.diff", "Dict.intersect", "Dict.update" ]


arities : List ( Int, List String )
arities =
    [ ( 0, [ "Dict.empty" ] )
    , ( 1, [ "Dict.fromList", "Dict.toList", "Dict.keys", "Dict.values", "Dict.size", "Dict.isEmpty" ] )
    , ( 3, [ "Dict.insert", "Dict.foldl", "Dict.foldr", "Dict.update" ] )
    ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run core globals name args =
    case ( name, args ) of
        ( "Dict.empty", [] ) ->
            Just (Ok (mkDict []))

        ( "Dict.singleton", [ k, v ] ) ->
            Just (Ok (mkDict [ VTup [ k, v ] ]))

        ( "Dict.fromList", [ VList ps ] ) ->
            Just (Ok (List.foldl (\p acc -> dictInsertPair p acc) (mkDict []) ps))

        ( "Dict.toList", [ d ] ) ->
            Just (Ok (VList (dictPairs d)))

        ( "Dict.keys", [ d ] ) ->
            Just (Ok (VList (List.filterMap pairKey (dictPairs d))))

        ( "Dict.values", [ d ] ) ->
            Just (Ok (VList (List.filterMap pairValue (dictPairs d))))

        ( "Dict.size", [ d ] ) ->
            Just (Ok (VNum (toFloat (List.length (dictPairs d)))))

        ( "Dict.isEmpty", [ d ] ) ->
            Just (Ok (VBool (List.isEmpty (dictPairs d))))

        ( "Dict.member", [ k, d ] ) ->
            Just (Ok (VBool (dictGet k (dictPairs d) /= Nothing)))

        ( "Dict.get", [ k, d ] ) ->
            Just (Ok (maybeValue (dictGet k (dictPairs d))))

        ( "Dict.insert", [ k, v, d ] ) ->
            Just (Ok (mkDict (dictSet k v (dictPairs d))))

        ( "Dict.remove", [ k, d ] ) ->
            Just (Ok (mkDict (List.filter (\p -> not (pairKeyEq k p)) (dictPairs d))))

        ( "Dict.map", [ f, d ] ) ->
            Just (mapDict core globals f (dictPairs d))

        ( "Dict.filter", [ f, d ] ) ->
            Just (filterDict core globals f (dictPairs d))

        ( "Dict.foldl", [ f, acc, d ] ) ->
            Just (foldlDict core globals f acc (dictPairs d))

        ( "Dict.foldr", [ f, acc, d ] ) ->
            Just (foldlDict core globals f acc (List.reverse (dictPairs d)))

        ( "Dict.partition", [ f, d ] ) ->
            -- (matching, non-matching) by the key/value predicate, preserving order
            Just (partitionDict core globals f (dictPairs d))

        ( "Dict.union", [ a, b ] ) ->
            -- Left-biased: a's entries win on a key collision.
            let
                aKeys =
                    List.filterMap pairKey (dictPairs a)
            in
            Just (Ok (mkDict (dictPairs a ++ List.filter (\p -> not (List.any (\k -> pairKeyEq k p) aKeys)) (dictPairs b))))

        ( "Dict.diff", [ a, b ] ) ->
            let
                bKeys =
                    List.filterMap pairKey (dictPairs b)
            in
            Just (Ok (mkDict (List.filter (\p -> not (List.any (\k -> pairKeyEq k p) bKeys)) (dictPairs a))))

        ( "Dict.intersect", [ a, b ] ) ->
            let
                bKeys =
                    List.filterMap pairKey (dictPairs b)
            in
            Just (Ok (mkDict (List.filter (\p -> List.any (\k -> pairKeyEq k p) bKeys) (dictPairs a))))

        ( "Dict.update", [ k, f, d ] ) ->
            -- f : Maybe v -> Maybe v; Just replaces/inserts, Nothing removes the key.
            Just
                (core.apply globals f (maybeValue (dictGet k (dictPairs d)))
                    |> Result.map
                        (\res ->
                            case res of
                                VCtor "Just" [ v ] ->
                                    mkDict (dictSet k v (dictPairs d))

                                _ ->
                                    mkDict (List.filter (\p -> not (pairKeyEq k p)) (dictPairs d))
                        )
                )

        _ ->
            Nothing



-- The dict representation + assoc-list helpers (pure), then the apply-dependent map/filter/fold.


mkDict : List Value -> Value
mkDict pairs =
    VCtor "Dict" [ VList pairs ]


dictPairs : Value -> List Value
dictPairs v =
    case v of
        VCtor "Dict" [ VList ps ] ->
            ps

        _ ->
            []


pairKeyEq : Value -> Value -> Bool
pairKeyEq k p =
    case pairKey p of
        Just pk ->
            valueEq k pk

        Nothing ->
            False


dictGet : Value -> List Value -> Maybe Value
dictGet k pairs =
    case List.filter (pairKeyEq k) pairs of
        p :: _ ->
            pairValue p

        [] ->
            Nothing


dictSet : Value -> Value -> List Value -> List Value
dictSet k v pairs =
    List.filter (\p -> not (pairKeyEq k p)) pairs ++ [ VTup [ k, v ] ]


dictInsertPair : Value -> Value -> Value
dictInsertPair pair d =
    case pair of
        VTup [ k, v ] ->
            mkDict (dictSet k v (dictPairs d))

        _ ->
            d


mapDict : Core -> Globals -> Value -> List Value -> Result String Value
mapDict core globals f pairs =
    case pairs of
        [] ->
            Ok (mkDict [])

        (VTup [ k, v ]) :: rest ->
            core.apply globals f k
                |> Result.andThen (\g -> core.apply globals g v)
                |> Result.andThen
                    (\v2 -> mapDict core globals f rest |> Result.map (\d -> mkDict (VTup [ k, v2 ] :: dictPairs d)))

        _ :: rest ->
            mapDict core globals f rest


filterDict : Core -> Globals -> Value -> List Value -> Result String Value
filterDict core globals f pairs =
    case pairs of
        [] ->
            Ok (mkDict [])

        ((VTup [ k, v ]) as p) :: rest ->
            core.apply globals f k
                |> Result.andThen (\g -> core.apply globals g v)
                |> Result.andThen
                    (\keep ->
                        filterDict core globals f rest
                            |> Result.map
                                (\d ->
                                    if keep == VBool True then
                                        mkDict (p :: dictPairs d)

                                    else
                                        d
                                )
                    )

        _ :: rest ->
            filterDict core globals f rest


partitionDict : Core -> Globals -> Value -> List Value -> Result String Value
partitionDict core globals f pairs =
    case pairs of
        [] ->
            Ok (VTup [ mkDict [], mkDict [] ])

        ((VTup [ k, v ]) as p) :: rest ->
            core.apply globals f k
                |> Result.andThen (\g -> core.apply globals g v)
                |> Result.andThen
                    (\keep ->
                        partitionDict core globals f rest
                            |> Result.map
                                (\split ->
                                    case split of
                                        VTup [ yes, no ] ->
                                            if keep == VBool True then
                                                VTup [ mkDict (p :: dictPairs yes), no ]

                                            else
                                                VTup [ yes, mkDict (p :: dictPairs no) ]

                                        _ ->
                                            split
                                )
                    )

        _ :: rest ->
            partitionDict core globals f rest


foldlDict : Core -> Globals -> Value -> Value -> List Value -> Result String Value
foldlDict core globals f acc pairs =
    case pairs of
        [] ->
            Ok acc

        (VTup [ k, v ]) :: rest ->
            core.apply globals f k
                |> Result.andThen (\g -> core.apply globals g v)
                |> Result.andThen (\h -> core.apply globals h acc)
                |> Result.andThen (\acc2 -> foldlDict core globals f acc2 rest)

        _ :: rest ->
            foldlDict core globals f acc rest
