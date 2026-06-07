module Eval.Core exposing (Core, Processor, asList, asNum, charOf, keepJust, maybeValue, pairKey, pairValue, valueCompare, valueEq)

{-| The shared boundary that lets the interpreter's builtins be split into one focused module per Elm
module (`Eval.String`, `Eval.List`, …) without an import cycle back to `Eval`.

  - `Core` is the set of interpreter capabilities a builtin module is handed: the higher-order
    combinators (`apply`, `mapValues`, …) that need the core `evalExpr` loop, which lives in `Eval`.
    `Eval` builds one `Core` and passes it to each module's `processor`.
  - `Processor` is the uniform "interface" each builtin module produces (`MODULE.processor core`): the
    builtin `names` it owns, their non-default `arities`, and a `run` that dispatches them. `Eval`
    aggregates `builtinNames`/`arityTable` and the runtime dispatch from a `Dict` of these.

This module also holds the small *pure* value helpers the builtins share (no `apply` needed), so both
`Eval` and the per-module files can import them directly. It is a leaf (only `Lang`), so even
`Eval.Render` — which holds the rendering helpers `renderValue`/`renderStr` — can import the types.

-}

import Lang exposing (Globals, Value(..))


{-| Interpreter capabilities injected into each builtin module — the parts that depend on the core
`evalExpr` loop (so they can't live in a leaf module). `Eval` constructs this once. -}
type alias Core =
    { apply : Globals -> Value -> Value -> Result String Value
    , applyAll : Globals -> Value -> List Value -> Result String Value
    , mapValues : Globals -> Value -> List Value -> Result String (List Value)
    , filterValues : Globals -> Value -> List Value -> Result String (List Value)
    , foldlValues : Globals -> Value -> Value -> List Value -> Result String Value
    }


{-| What a builtin module contributes, as a single record (so adding a module is one import + one map
entry). `run core name args` returns `Nothing` when `name` isn't one of this module's builtins.

`run` takes the `Core` as a parameter rather than the record closing over it: that keeps a `Processor`
a *core-free* value, so `Eval` can aggregate `names`/`arities` from the processor map without the map
transitively depending on `apply` (which depends back on the builtin tables). Capturing `core` in the
record instead creates a value-initialisation cycle that the eager JS backend can't order. -}
type alias Processor =
    { names : List String
    , arities : List ( Int, List String )
    , run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
    }


{-| Wraps a `Maybe` result as the interpreter's `Just`/`Nothing` value. -}
maybeValue : Maybe Value -> Value
maybeValue m =
    case m of
        Just v ->
            VCtor "Just" [ v ]

        Nothing ->
            VCtor "Nothing" []


{-| The `Char` a value holds, if it is one (for rebuilding strings from char lists). -}
charOf : Value -> Maybe Char
charOf v =
    case v of
        VChar c ->
            Just c

        _ ->
            Nothing


{-| The `Float` a value holds, if it is a number. -}
asNum : Value -> Maybe Float
asNum v =
    case v of
        VNum n ->
            Just n

        _ ->
            Nothing


{-| A value as a list of values (non-lists are empty) — for `List.concat`/`concatMap`. -}
asList : Value -> List Value
asList v =
    case v of
        VList xs ->
            xs

        _ ->
            []


{-| Unwraps a `Just`-tagged value, dropping `Nothing`s — for `List.filterMap`. -}
keepJust : Value -> Maybe Value
keepJust v =
    case v of
        VCtor "Just" [ x ] ->
            Just x

        _ ->
            Nothing


{-| Structural equality of two interpreter values (the semantics of `==`). Self-contained so the
collection builtins (`List.member`, `Dict`, `Set`) and `Eval`'s `==` share one definition. -}
valueEq : Value -> Value -> Bool
valueEq a b =
    case ( a, b ) of
        ( VNum x, VNum y ) ->
            x == y

        ( VBool x, VBool y ) ->
            x == y

        ( VStr x, VStr y ) ->
            x == y

        ( VChar x, VChar y ) ->
            x == y

        ( VList x, VList y ) ->
            listEq x y

        ( VCtor n1 a1, VCtor n2 a2 ) ->
            n1 == n2 && listEq a1 a2

        ( VTup x, VTup y ) ->
            listEq x y

        ( VRecord f1, VRecord f2 ) ->
            List.length f1 == List.length f2 && List.all (fieldMatches f2) f1

        _ ->
            False


fieldMatches : List ( String, Value ) -> ( String, Value ) -> Bool
fieldMatches other pair =
    case List.head (List.filter (\( k, _ ) -> k == Tuple.first pair) other) of
        Just ( _, v ) ->
            valueEq (Tuple.second pair) v

        Nothing ->
            False


listEq : List Value -> List Value -> Bool
listEq xs ys =
    case ( xs, ys ) of
        ( [], [] ) ->
            True

        ( x :: xrest, y :: yrest ) ->
            valueEq x y && listEq xrest yrest

        _ ->
            False


{-| Ordering of two values (for `List.sort`/`compare`); numbers, strings, bools, then tuples
lexicographically, and anything else equal. -}
valueCompare : Value -> Value -> Order
valueCompare a b =
    case ( a, b ) of
        ( VNum x, VNum y ) ->
            compare x y

        ( VStr x, VStr y ) ->
            compare x y

        ( VBool x, VBool y ) ->
            compare (boolRank x) (boolRank y)

        ( VTup (x :: xrest), VTup (y :: yrest) ) ->
            case valueCompare x y of
                EQ ->
                    valueCompare (VTup xrest) (VTup yrest)

                ord ->
                    ord

        _ ->
            EQ


boolRank : Bool -> Int
boolRank b =
    if b then
        1

    else
        0


{-| The key (first element) of a 2-tuple value, for `List.unzip`/`Dict`. -}
pairKey : Value -> Maybe Value
pairKey p =
    case p of
        VTup [ k, _ ] ->
            Just k

        _ ->
            Nothing


{-| The value (second element) of a 2-tuple value. -}
pairValue : Value -> Maybe Value
pairValue p =
    case p of
        VTup [ _, v ] ->
            Just v

        _ ->
            Nothing
