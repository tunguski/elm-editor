module Eval.Maybe exposing (processor)

{-| The interpreter's `Maybe.*` builtins, as an {@link Eval.Core.Processor}. -}

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
    [ "Maybe.withDefault", "Maybe.map", "Maybe.andThen", "Maybe.map2", "Maybe.map3", "Maybe.map4", "Maybe.map5" ]


arities : List ( Int, List String )
arities =
    [ ( 3, [ "Maybe.map2" ] ), ( 4, [ "Maybe.map3" ] ), ( 5, [ "Maybe.map4" ] ), ( 6, [ "Maybe.map5" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run core globals name args =
    case ( name, args ) of
        ( "Maybe.withDefault", [ dflt, v ] ) ->
            Just
                (case v of
                    VCtor "Just" [ x ] ->
                        Ok x

                    _ ->
                        Ok dflt
                )

        ( "Maybe.map", [ f, v ] ) ->
            Just
                (case v of
                    VCtor "Just" [ x ] ->
                        core.apply globals f x |> Result.map (\y -> VCtor "Just" [ y ])

                    _ ->
                        Ok (VCtor "Nothing" [])
                )

        ( "Maybe.andThen", [ f, v ] ) ->
            Just
                (case v of
                    VCtor "Just" [ x ] ->
                        core.apply globals f x

                    _ ->
                        Ok (VCtor "Nothing" [])
                )

        ( "Maybe.map2", [ f, va, vb ] ) ->
            Just
                (case ( va, vb ) of
                    ( VCtor "Just" [ a ], VCtor "Just" [ b ] ) ->
                        core.apply globals f a |> Result.andThen (\g -> core.apply globals g b) |> Result.map (\y -> VCtor "Just" [ y ])

                    _ ->
                        Ok (VCtor "Nothing" [])
                )

        ( "Maybe.map3", [ f, a, b, c ] ) ->
            Just (maybeMapN core globals f [ a, b, c ])

        ( "Maybe.map4", [ f, a, b, c, d ] ) ->
            Just (maybeMapN core globals f [ a, b, c, d ])

        ( "Maybe.map5", [ f, a, b, c, d, e ] ) ->
            Just (maybeMapN core globals f [ a, b, c, d, e ])

        _ ->
            Nothing


{-| `Maybe.mapN`: if every argument is `Just`, apply `f` to the unwrapped values; else `Nothing`. -}
maybeMapN : Core -> Globals -> Value -> List Value -> Result String Value
maybeMapN core globals f margs =
    case allJust margs of
        Just xs ->
            List.foldl (\x acc -> acc |> Result.andThen (\g -> core.apply globals g x)) (Ok f) xs
                |> Result.map (\y -> VCtor "Just" [ y ])

        Nothing ->
            Ok (VCtor "Nothing" [])


allJust : List Value -> Maybe (List Value)
allJust margs =
    case margs of
        [] ->
            Just []

        (VCtor "Just" [ x ]) :: rest ->
            Maybe.map (\xs -> x :: xs) (allJust rest)

        _ ->
            Nothing
