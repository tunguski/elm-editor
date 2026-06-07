module EvalCore exposing (Core, Processor, charOf, maybeValue, renderStr)

{-| The shared boundary that lets the interpreter's builtins be split into one focused module per Elm
module (`EvalString`, `EvalList`, …) without an import cycle back to `Eval`.

  - `Core` is the set of interpreter capabilities a builtin module is handed: the higher-order
    combinators (`apply`, `mapValues`, …) that need the core `evalExpr` loop, which lives in `Eval`.
    `Eval` builds one `Core` and passes it to each module's `processor`.
  - `Processor` is the uniform "interface" each builtin module produces (`MODULE.processor core`): the
    builtin `names` it owns, their non-default `arities`, and a `run` that dispatches them. `Eval`
    aggregates `builtinNames`/`arityTable` and the runtime dispatch from a `Dict` of these.

This module also holds the small *pure* value helpers the builtins share (no `apply` needed), so both
`Eval` and the per-module files can import them directly.

-}

import EvalRender
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


{-| A value as the `String` it stringifies to — itself if it's already a string, else its rendered
form (for `String.join`/`String.concat` over non-string lists). -}
renderStr : Value -> String
renderStr v =
    case v of
        VStr s ->
            s

        _ ->
            EvalRender.renderValue v


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
