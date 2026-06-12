module Eval.Lazy exposing (processor)

{-| The interpreter's `Html.Lazy`/`Svg.Lazy` `lazyN` builtins, as an {@link Eval.Core.Processor}. The
interpreter re-renders every frame, so `lazy` has nothing to memoise — it just forces, applying the
view function to its arguments. -}

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
    [ "lazy", "lazy2", "lazy3", "lazy4", "lazy5", "lazy6", "lazy7", "lazy8" ]
        ++ [ "Html.Lazy.lazy", "Html.Lazy.lazy2", "Html.Lazy.lazy3", "Html.Lazy.lazy4", "Html.Lazy.lazy5", "Html.Lazy.lazy6", "Html.Lazy.lazy7", "Html.Lazy.lazy8" ]
        ++ [ "Svg.Lazy.lazy", "Svg.Lazy.lazy2", "Svg.Lazy.lazy3", "Svg.Lazy.lazy4", "Svg.Lazy.lazy5", "Svg.Lazy.lazy6", "Svg.Lazy.lazy7", "Svg.Lazy.lazy8" ]


arities : List ( Int, List String )
arities =
    -- `lazyN` takes a view function plus N arguments.
    [ ( 2, [ "lazy", "Html.Lazy.lazy", "Svg.Lazy.lazy" ] )
    , ( 3, [ "lazy2", "Html.Lazy.lazy2", "Svg.Lazy.lazy2" ] )
    , ( 4, [ "lazy3", "Html.Lazy.lazy3", "Svg.Lazy.lazy3" ] )
    , ( 5, [ "lazy4", "Html.Lazy.lazy4", "Svg.Lazy.lazy4" ] )
    , ( 6, [ "lazy5", "Html.Lazy.lazy5", "Svg.Lazy.lazy5" ] )
    , ( 7, [ "lazy6", "Html.Lazy.lazy6", "Svg.Lazy.lazy6" ] )
    , ( 8, [ "lazy7", "Html.Lazy.lazy7", "Svg.Lazy.lazy7" ] )
    , ( 9, [ "lazy8", "Html.Lazy.lazy8", "Svg.Lazy.lazy8" ] )
    ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run core globals name args =
    if isLazyBuiltin name then
        case args of
            f :: rest ->
                Just (core.applyAll globals f rest)

            [] ->
                Just (Err (name ++ ": missing view function"))

    else
        Nothing


{-| Whether a name is one of the `lazyN` family (qualified `Html.Lazy.lazy2` or exposed `lazy2`) —
the last dotted segment is what matters. -}
isLazyBuiltin : String -> Bool
isLazyBuiltin name =
    List.member
        (String.split "." name |> List.reverse |> List.head |> Maybe.withDefault name)
        [ "lazy", "lazy2", "lazy3", "lazy4", "lazy5", "lazy6", "lazy7", "lazy8" ]
