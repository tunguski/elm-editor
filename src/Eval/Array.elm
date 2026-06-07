module Eval.Array exposing (processor)

{-| The interpreter's `Array.*` builtins, as an {@link Eval.Core.Processor}. An array is a 0-indexed
sequence, `VCtor "Array" [ VList elems ]`. -}

import Eval.Core exposing (Core, Processor, maybeValue)
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


names : List String
names =
    [ "Array.empty", "Array.initialize", "Array.repeat", "Array.fromList", "Array.toList", "Array.toIndexedList", "Array.get", "Array.set", "Array.push" ]
        ++ [ "Array.append", "Array.length", "Array.isEmpty", "Array.slice", "Array.map", "Array.indexedMap", "Array.foldl", "Array.foldr", "Array.filter" ]


arities : List ( Int, List String )
arities =
    [ ( 0, [ "Array.empty" ] )
    , ( 1, [ "Array.fromList", "Array.toList", "Array.toIndexedList", "Array.length", "Array.isEmpty" ] )
    , ( 3, [ "Array.foldl", "Array.foldr", "Array.set", "Array.slice" ] )
    ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run core globals name args =
    case ( name, args ) of
        ( "Array.empty", [] ) ->
            Just (Ok (mkArray []))

        ( "Array.fromList", [ VList xs ] ) ->
            Just (Ok (mkArray xs))

        ( "Array.toList", [ a ] ) ->
            Just (Ok (VList (arrayElems a)))

        ( "Array.toIndexedList", [ a ] ) ->
            Just (Ok (VList (List.indexedMap (\i x -> VTup [ VNum (toFloat i), x ]) (arrayElems a))))

        ( "Array.length", [ a ] ) ->
            Just (Ok (VNum (toFloat (List.length (arrayElems a)))))

        ( "Array.isEmpty", [ a ] ) ->
            Just (Ok (VBool (List.isEmpty (arrayElems a))))

        ( "Array.repeat", [ VNum n, x ] ) ->
            Just (Ok (mkArray (List.repeat (round n) x)))

        ( "Array.initialize", [ VNum n, f ] ) ->
            Just
                (core.mapValues globals f (List.map (\i -> VNum (toFloat i)) (List.range 0 (round n - 1)))
                    |> Result.map mkArray
                )

        ( "Array.get", [ VNum i, a ] ) ->
            let
                xs =
                    arrayElems a

                idx =
                    round i
            in
            if idx >= 0 && idx < List.length xs then
                Just (Ok (maybeValue (List.head (List.drop idx xs))))

            else
                Just (Ok (VCtor "Nothing" []))

        ( "Array.set", [ VNum i, x, a ] ) ->
            let
                idx =
                    round i
            in
            Just
                (Ok
                    (mkArray
                        (List.indexedMap
                            (\j y ->
                                if j == idx then
                                    x

                                else
                                    y
                            )
                            (arrayElems a)
                        )
                    )
                )

        ( "Array.push", [ x, a ] ) ->
            Just (Ok (mkArray (arrayElems a ++ [ x ])))

        ( "Array.append", [ a, b ] ) ->
            Just (Ok (mkArray (arrayElems a ++ arrayElems b)))

        ( "Array.slice", [ VNum from, VNum to, a ] ) ->
            Just (Ok (mkArray (arraySlice (round from) (round to) (arrayElems a))))

        ( "Array.map", [ f, a ] ) ->
            Just (core.mapValues globals f (arrayElems a) |> Result.map mkArray)

        ( "Array.indexedMap", [ f, a ] ) ->
            Just (indexedMapValues core globals f 0 (arrayElems a) |> Result.map mkArray)

        ( "Array.filter", [ f, a ] ) ->
            Just (core.filterValues globals f (arrayElems a) |> Result.map mkArray)

        ( "Array.foldl", [ f, acc, a ] ) ->
            Just (core.foldlValues globals f acc (arrayElems a))

        ( "Array.foldr", [ f, acc, a ] ) ->
            Just (core.foldlValues globals f acc (List.reverse (arrayElems a)))

        _ ->
            Nothing


mkArray : List Value -> Value
mkArray xs =
    VCtor "Array" [ VList xs ]


arrayElems : Value -> List Value
arrayElems v =
    case v of
        VCtor "Array" [ VList xs ] ->
            xs

        _ ->
            []


arraySlice : Int -> Int -> List Value -> List Value
arraySlice from to xs =
    let
        len =
            List.length xs

        norm i =
            if i < 0 then
                Basics.max 0 (len + i)

            else
                Basics.min i len

        lo =
            norm from

        hi =
            norm to
    in
    xs |> List.drop lo |> List.take (Basics.max 0 (hi - lo))


indexedMapValues : Core -> Globals -> Value -> Int -> List Value -> Result String (List Value)
indexedMapValues core globals f i xs =
    case xs of
        [] ->
            Ok []

        x :: rest ->
            core.apply globals f (VNum (toFloat i))
                |> Result.andThen (\g -> core.apply globals g x)
                |> Result.andThen (\y -> indexedMapValues core globals f (i + 1) rest |> Result.map (\ys -> y :: ys))
