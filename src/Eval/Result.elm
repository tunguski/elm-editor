module Eval.Result exposing (processor)

{-| The interpreter's `Result.*` builtins, as an {@link Eval.Core.Processor}. -}

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
    [ "Result.withDefault", "Result.map", "Result.andThen", "Result.toMaybe", "Result.mapError", "Result.fromMaybe", "Result.map2", "Result.map3", "Result.map4", "Result.map5" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "Result.toMaybe" ] ), ( 3, [ "Result.map2" ] ), ( 4, [ "Result.map3" ] ), ( 5, [ "Result.map4" ] ), ( 6, [ "Result.map5" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run core globals name args =
    case ( name, args ) of
        ( "Result.withDefault", [ dflt, v ] ) ->
            Just
                (case v of
                    VCtor "Ok" [ x ] ->
                        Ok x

                    _ ->
                        Ok dflt
                )

        ( "Result.map", [ f, v ] ) ->
            Just
                (case v of
                    VCtor "Ok" [ x ] ->
                        core.apply globals f x |> Result.map (\y -> VCtor "Ok" [ y ])

                    _ ->
                        Ok v
                )

        ( "Result.andThen", [ f, v ] ) ->
            Just
                (case v of
                    VCtor "Ok" [ x ] ->
                        core.apply globals f x

                    _ ->
                        Ok v
                )

        ( "Result.toMaybe", [ v ] ) ->
            Just
                (case v of
                    VCtor "Ok" [ x ] ->
                        Ok (VCtor "Just" [ x ])

                    _ ->
                        Ok (VCtor "Nothing" [])
                )

        ( "Result.mapError", [ f, v ] ) ->
            Just
                (case v of
                    VCtor "Err" [ x ] ->
                        core.apply globals f x |> Result.map (\y -> VCtor "Err" [ y ])

                    _ ->
                        Ok v
                )

        ( "Result.fromMaybe", [ err, v ] ) ->
            Just
                (case v of
                    VCtor "Just" [ x ] ->
                        Ok (VCtor "Ok" [ x ])

                    _ ->
                        Ok (VCtor "Err" [ err ])
                )

        ( "Result.map2", [ f, va, vb ] ) ->
            Just
                (case ( va, vb ) of
                    ( VCtor "Ok" [ a ], VCtor "Ok" [ b ] ) ->
                        core.apply globals f a |> Result.andThen (\g -> core.apply globals g b) |> Result.map (\y -> VCtor "Ok" [ y ])

                    ( VCtor "Err" [ x ], _ ) ->
                        Ok (VCtor "Err" [ x ])

                    ( _, err ) ->
                        Ok err
                )

        ( "Result.map3", [ f, va, vb, vc ] ) ->
            Just
                (case ( va, vb, vc ) of
                    ( VCtor "Ok" [ a ], VCtor "Ok" [ b ], VCtor "Ok" [ c ] ) ->
                        core.apply globals f a
                            |> Result.andThen (\g -> core.apply globals g b)
                            |> Result.andThen (\h -> core.apply globals h c)
                            |> Result.map (\y -> VCtor "Ok" [ y ])

                    ( VCtor "Err" [ x ], _, _ ) ->
                        Ok (VCtor "Err" [ x ])

                    ( _, VCtor "Err" [ x ], _ ) ->
                        Ok (VCtor "Err" [ x ])

                    ( _, _, err ) ->
                        Ok err
                )

        ( "Result.map4", [ f, a, b, c, d ] ) ->
            Just (resultMapN core globals f [ a, b, c, d ])

        ( "Result.map5", [ f, a, b, c, d, e ] ) ->
            Just (resultMapN core globals f [ a, b, c, d, e ])

        _ ->
            Nothing


{-| `Result.mapN`: if every argument is `Ok`, apply `f` to the unwrapped values; else the first
`Err`. -}
resultMapN : Core -> Globals -> Value -> List Value -> Result String Value
resultMapN core globals f rs =
    case allOk rs of
        Ok xs ->
            core.applyAll globals f xs |> Result.map (\y -> VCtor "Ok" [ y ])

        Err e ->
            Ok (VCtor "Err" [ e ])


allOk : List Value -> Result Value (List Value)
allOk rs =
    case rs of
        [] ->
            Ok []

        (VCtor "Ok" [ x ]) :: rest ->
            allOk rest |> Result.map (\xs -> x :: xs)

        (VCtor "Err" [ e ]) :: _ ->
            Err e

        _ :: rest ->
            allOk rest
