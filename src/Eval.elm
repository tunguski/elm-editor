module Eval exposing (eval, evalProject, debugSteps, lookup, renderValue, appInit, appUpdate, appView, hasApp, renderProgram, mainValue, applyHandler, appInitCmd, appUpdateCmd, appSubscription, appAnimation, appSubHandler, runEventDecoder, randomCmd, applyMsgIn, gameInitMem, gameView, gameStep, httpCmd, httpResult, fileSelectCmd, fileSelected, taskResult, appUpdateCmdOf, appViewOf, appSubscriptionOf, appAnimationOf, appSubHandlerOf, applyMsgInOf)

{-| The evaluator for the interpreted language. Global (top-level) definitions are threaded through
evaluation so all definitions across the project's files form one mutually-recursive scope. Public
entry points: `eval` (one expression), `evalProject` (entry expression against all files) and
`debugSteps` (fold messages through update for the time-travel debugger). -}

import Bitwise
import Dict
import Eval.Array
import Eval.Basics
import Eval.Bitwise
import Eval.Browser
import Eval.Char
import Eval.Core exposing (Core, Processor, asList, asNum, keepJust, maybeValue, pairKey, pairValue, valueCompare, valueEq)
import Eval.Debug
import Eval.Dict
import Eval.Encode
import Eval.Events
import Eval.File
import Eval.Http
import Eval.Json
import Eval.Lazy
import Eval.List
import Eval.Math
import Eval.Maybe
import Eval.Playground
import Eval.Random
import Eval.Render
import Eval.Result
import Eval.Set
import Eval.String
import Eval.Task
import Eval.Time
import Eval.Url
import Eval.Tuple
import Eval.WebGL
import Lang exposing (Decl, Env, Expr(..), Globals, Pattern(..), Value(..))
import Lexer exposing (tokenize)
import Parser exposing (parse, parseProject)
import Set exposing (Set)


{-| Native builtins available to interpreted programs (resolved when a name is in neither the local
scope nor the project's top-level definitions), as a `Set` so membership is O(1) — it is checked on
every name that isn't a local/global, including in hot interpreter loops. Every builtin is owned by a
{@link Eval.Core.Processor}, so this is exactly the processors' aggregated names. -}
builtins : Set String
builtins =
    Set.fromList builtinNames


builtinNames : List String
builtinNames =
    processorNames


{-| `Browser.Events` subscription functions, recognised by their (unqualified) field name so the
import alias (`as E`, `as Events`, …) doesn't matter, then handled by {@link Eval.Events}. -}
browserEventSubs : List String
browserEventSubs =
    [ "onAnimationFrameDelta", "onAnimationFrame", "onResize", "onMouseMove", "onMouseDown", "onMouseUp", "onKeyDown", "onKeyUp", "onKeyPress", "onVisibilityChange" ]


{-| `Json.Decode` function names, recognised after an import alias (`as D`, `as Decode`, …) so
`D.succeed`, `Decode.at`, … resolve to the bare decoder builtin regardless of the alias. -}
jsonDecodeNames : List String
jsonDecodeNames =
    [ "succeed", "map", "map2", "map3", "map4", "map5", "map6", "map7", "map8", "field", "at", "list", "oneOf", "oneOrMore", "andThen", "nullable", "string", "int", "float", "bool" ]


{-| How many arguments a builtin consumes before it runs — an O(1) lookup (built once), since it is
queried on every argument applied to every builtin. The default (a name not in the table) is 2. -}
arity : String -> Int
arity name =
    Dict.get name arityTable |> Maybe.withDefault 2


arityTable : Dict String Int
arityTable =
    processorArities
        |> List.concatMap (\( n, names ) -> List.map (\nm -> ( nm, n )) names)
        |> Dict.fromList


evalExpr : Globals -> Env -> Expr -> Result String Value
evalExpr globals env expr =
    case expr of
        Num n ->
            Ok (VNum n)

        Str s ->
            Ok (VStr s)

        CharLit ch ->
            Ok (VChar ch)

        Boolean b ->
            Ok (VBool b)

        Var name ->
            case lookup name env of
                Just v ->
                    Ok v

                Nothing ->
                    case Dict.get name globals of
                        Just decl ->
                            if List.isEmpty decl.params then
                                evalExpr globals [] decl.body

                            else
                                Ok (VClosure decl.params decl.body [])

                        Nothing ->
                            if name == "pi" then
                                Ok (VNum pi)

                            else if name == "e" then
                                Ok (VNum e)

                            else if name == "Encode.null" then
                                Ok (VCtor "Null" [])

                            else if List.member name [ "string", "int", "float", "bool" ] then
                                -- Json.Decode primitive decoders (exposed unqualified by the quotes
                                -- example); locals/globals are checked first, so a same-named binding
                                -- still shadows them.
                                Ok (VCtor ("Dec." ++ name) [])

                            else
                                case Eval.Playground.playgroundColor name of
                                    Just hex ->
                                        Ok (VStr hex)

                                    Nothing ->
                                        if Set.member name builtins then
                                            Ok (VBuiltin name [])

                                        else
                                            Err ("undefined variable: " ++ name)

        Ctor name ->
            -- A `type alias` record constructor is registered as a global; everything else
            -- (custom-type constructors) builds a tagged value.
            case Dict.get name globals of
                Just decl ->
                    if List.isEmpty decl.params then
                        evalExpr globals [] decl.body

                    else
                        Ok (VClosure decl.params decl.body [])

                Nothing ->
                    Ok (VCtor name [])

        Case subject branches ->
            evalExpr globals env subject
                |> Result.andThen (\v -> evalCase globals env v branches)

        ListE items ->
            evalList globals env items []

        Neg inner ->
            evalExpr globals env inner
                |> Result.andThen
                    (\v ->
                        case v of
                            VNum n ->
                                Ok (VNum (negate n))

                            _ ->
                                Err "cannot negate a non-number"
                    )

        If cond then_ else_ ->
            evalExpr globals env cond
                |> Result.andThen
                    (\v ->
                        case v of
                            VBool True ->
                                evalExpr globals env then_

                            VBool False ->
                                evalExpr globals env else_

                            _ ->
                                Err "if condition must be a Bool"
                    )

        Let name boundExpr body ->
            case boundExpr of
                Lam params lamBody ->
                    evalExpr globals (( name, VRec name params lamBody env ) :: env) body

                _ ->
                    evalExpr globals env boundExpr
                        |> Result.andThen (\v -> evalExpr globals (( name, v ) :: env) body)

        Lam params body ->
            Ok (VClosure params body env)

        App fn arg ->
            evalExpr globals env fn
                |> Result.andThen
                    (\fv ->
                        evalExpr globals env arg
                            |> Result.andThen (\av -> applyValue globals fv av)
                    )

        BinOp op l r ->
            evalExpr globals env l
                |> Result.andThen
                    (\lv ->
                        evalExpr globals env r
                            |> Result.andThen (\rv -> applyOp op lv rv)
                    )

        RecordLit fields ->
            evalFields globals env fields []

        RecordGet target field ->
            case target of
                -- A qualified name like `String.fromInt` parses as RecordGet (Ctor "String")
                -- "fromInt"; resolve it to the matching builtin when there is one.
                Ctor moduleName ->
                    let
                        qualified =
                            moduleName ++ "." ++ field
                    in
                    if (moduleName == "Cmd" || moduleName == "Sub" || moduleName == "Task") && not (Set.member qualified builtins) then
                        -- Effects with no editor builtin are opaque no-ops (Cmd.none, Sub.none, …);
                        -- ones the editor does run (e.g. Task.perform) fall through to the builtin.
                        Ok (VCtor moduleName [])

                    else if qualified == "Time.now" then
                        -- A Task yielding the current time. The pure interpreter has no clock, so it
                        -- resolves to epoch 0; a `Time.every` subscription (which the editor drives
                        -- with the real clock) then advances it — enough for the clock/time examples.
                        Ok (VCtor "Task.value" [ VNum 0 ])

                    else if qualified == "Time.here" then
                        -- A Task yielding the local Zone, modelled (like Time.utc) as a 0 offset.
                        Ok (VCtor "Task.value" [ VNum 0 ])

                    else if qualified == "Time.utc" then
                        Ok (VNum 0)
                        -- a Zone, modelled as a 0 offset

                    else if qualified == "File.decoder" then
                        -- The Json.Decode decoder for a dropped/selected File (used by image-previews).
                        Ok (VCtor "Dec.file" [])

                    else if qualified == "Encode.null" then
                        -- `Json.Encode.null` written qualified (e.g. `Encode.null`); serialises to JSON null.
                        Ok (VCtor "Null" [])

                    else if moduleName == "Select" && field == "files" then
                        -- `File.Select as Select` aliased: Select.files mimes toMsg opens a file picker.
                        Ok (VBuiltin "File.Select.files" [])

                    else if moduleName == "Select" && field == "file" then
                        Ok (VBuiltin "File.Select.file" [])

                    else if Set.member qualified builtins then
                        -- A zero-argument builtin (e.g. `Dict.empty`) evaluates immediately.
                        if arity qualified == 0 then
                            runBuiltin globals qualified []

                        else
                            Ok (VBuiltin qualified [])

                    else if List.member field browserEventSubs then
                        -- A Browser.Events subscription under any import alias (E, Events, …): resolve
                        -- by the bare field name so `E.onAnimationFrameDelta`, `Events.onResize`, … all work.
                        Ok (VBuiltin field [])

                    else if List.member field [ "string", "int", "float", "bool" ] && List.member field jsonDecodeNames then
                        -- A Json.Decode primitive decoder under an alias (`D.string`, `Decode.int`).
                        Ok (VCtor ("Dec." ++ field) [])

                    else if List.member field jsonDecodeNames then
                        -- A Json.Decode combinator under an alias (`D.succeed`, `Decode.at`, …).
                        Ok (VBuiltin field [])

                    else if Set.member field builtins then
                        -- A builtin referenced under its module name (e.g. `Html.text`, `Html.div`,
                        -- `Svg.circle`) where the file exposes only the type, not the function — as
                        -- thwomp does with `import Html exposing (Html)` then `Html.text "…"`.
                        -- Resolve it the same as the bare builtin `field`.
                        if arity field == 0 then
                            runBuiltin globals field []

                        else
                            Ok (VBuiltin field [])

                    else
                        Err ("unknown qualified name: " ++ qualified)

                _ ->
                    evalExpr globals env target
                        |> Result.andThen
                            (\v ->
                                case v of
                                    VRecord fs ->
                                        case lookup field fs of
                                            Just x ->
                                                Ok x

                                            Nothing ->
                                                Err ("record has no field ." ++ field)

                                    _ ->
                                        Err ("." ++ field ++ " needs a record")
                            )

        RecordUpdate name fields ->
            evalExpr globals env (Var name)
                |> Result.andThen
                    (\v ->
                        case v of
                            VRecord base ->
                                evalFields globals env fields []
                                    |> Result.andThen
                                        (\nv ->
                                            case nv of
                                                VRecord updates ->
                                                    Ok (VRecord (mergeFields base updates))

                                                _ ->
                                                    Err "internal: record update"
                                        )

                            _ ->
                                Err ("cannot update " ++ name ++ ": not a record")
                    )

        Tup items ->
            evalTupleItems globals env items []


evalTupleItems : Globals -> Env -> List Expr -> List Value -> Result String Value
evalTupleItems globals env items acc =
    case items of
        [] ->
            Ok (VTup (List.reverse acc))

        x :: rest ->
            evalExpr globals env x |> Result.andThen (\v -> evalTupleItems globals env rest (v :: acc))


evalFields : Globals -> Env -> List ( String, Expr ) -> List ( String, Value ) -> Result String Value
evalFields globals env fields acc =
    case fields of
        [] ->
            Ok (VRecord (List.reverse acc))

        ( name, expr ) :: rest ->
            evalExpr globals env expr
                |> Result.andThen (\v -> evalFields globals env rest (( name, v ) :: acc))


{-| Returns `base` with each field of `updates` replaced (or appended if new). -}
mergeFields : List ( String, Value ) -> List ( String, Value ) -> List ( String, Value )
mergeFields base updates =
    let
        replaced =
            List.map
                (\pair ->
                    case lookup (Tuple.first pair) updates of
                        Just v ->
                            ( Tuple.first pair, v )

                        Nothing ->
                            pair
                )
                base

        added =
            List.filter (\u -> lookup (Tuple.first u) base == Nothing) updates
    in
    replaced ++ added


evalList : Globals -> Env -> List Expr -> List Value -> Result String Value
evalList globals env items acc =
    case items of
        [] ->
            Ok (VList (List.reverse acc))

        x :: rest ->
            evalExpr globals env x |> Result.andThen (\v -> evalList globals env rest (v :: acc))


applyValue : Globals -> Value -> Value -> Result String Value
applyValue globals fn arg =
    case fn of
        VClosure params body closedEnv ->
            applyClosure globals params body closedEnv arg

        VRec name params body closedEnv ->
            applyClosure globals params body (( name, fn ) :: closedEnv) arg

        VCtor name args ->
            Ok (VCtor name (args ++ [ arg ]))

        VBuiltin name args ->
            let
                collected =
                    args ++ [ arg ]
            in
            if List.length collected >= arity name then
                runBuiltin globals name collected

            else
                Ok (VBuiltin name collected)

        _ ->
            Err "cannot apply a non-function value"



-- DICT (an association list of unique keys, wrapped as `VCtor "Dict" [ VList pairs ]`) --------------


-- SET (a list of unique values, wrapped as `VCtor "Set" [ VList elems ]`) ------------------------



{-| Applies the curried function `f` to each argument in turn (left to right). -}
applyAllValues : Globals -> Value -> List Value -> Result String Value
applyAllValues globals f args =
    List.foldl (\a acc -> acc |> Result.andThen (\g -> applyValue globals g a)) (Ok f) args


{-| The interpreter capabilities handed to each split-out builtin module (see {@link Eval.Core}). -}
core : Core
core =
    { apply = applyValue
    , applyAll = applyAllValues
    , mapValues = mapValues
    , filterValues = filterValues
    , foldlValues = foldlValues
    }


{-| The builtin modules split out of `Eval`, keyed by the Elm module they handle (the part of a
builtin name before the first dot). `runBuiltin` dispatches by name through {@link nameToProcessor};
`builtinNames`/`arityTable` are aggregated from their `.names`/`.arities`. -}
processors : Dict String Processor
processors =
    Dict.fromList
        [ ( "String", Eval.String.processor )
        , ( "Char", Eval.Char.processor )
        , ( "Bitwise", Eval.Bitwise.processor )
        , ( "Debug", Eval.Debug.processor )
        , ( "Tuple", Eval.Tuple.processor )
        , ( "Maybe", Eval.Maybe.processor )
        , ( "Result", Eval.Result.processor )
        , ( "List", Eval.List.processor )
        , ( "Set", Eval.Set.processor )
        , ( "Dict", Eval.Dict.processor )
        , ( "Array", Eval.Array.processor )
        , ( "Random", Eval.Random.processor )
        , ( "Time", Eval.Time.processor )
        , ( "Http", Eval.Http.processor )
        , ( "File", Eval.File.processor )
        , ( "Task", Eval.Task.processor )
        , ( "Browser", Eval.Browser.processor )
        , ( "Encode", Eval.Encode.processor )
        , ( "Url", Eval.Url.processor )
        ]


{-| Every split-out module's builtin names, folded into {@link builtinNames}. -}
processorNames : List String
processorNames =
    List.concatMap .names allProcessors


{-| Every split-out module's arity groups, folded into {@link arityTable}. -}
processorArities : List ( Int, List String )
processorArities =
    List.concatMap .arities allProcessors


{-| Every processor: the qualified ones (keyed by Elm module) plus the unqualified ones. -}
allProcessors : List Processor
allProcessors =
    Dict.values processors ++ unqualifiedProcessors


{-| Maps every builtin name straight to its owning processor, so dispatch is one `Dict.get` instead
of a `String.split` + a linear scan of the unqualified processors (each re-scanning ~120-entry tag
lists). Built first-wins over `allProcessors`, matching the old qualified-then-unqualified-in-order
precedence; on a `run` miss, `dispatchProcessor` still falls back to the linear scan for the same
behaviour as before. -}
nameToProcessor : Dict String Processor
nameToProcessor =
    List.foldl
        (\p acc ->
            List.foldl (\n a -> if Dict.member n a then a else Dict.insert n p a) acc p.names
        )
        Dict.empty
        allProcessors


{-| Processors for the unqualified builtins (no `Module.` prefix to key a `Dict` on): the playground
shapes/transforms, the JSON decoders, and the Html elements/attributes. Tried in order when no
qualified processor owns the name. -}
unqualifiedProcessors : List Processor
unqualifiedProcessors =
    [ Eval.Math.processor, Eval.Basics.processor, Eval.Playground.processor, Eval.Json.processor, Eval.Render.processor, Eval.Lazy.processor, Eval.Events.processor, Eval.WebGL.processor ]


{-| Runs a fully-applied builtin via its owning {@link Eval.Core.Processor}, found by name through
{@link nameToProcessor}. Every builtin lives in a processor now, so a miss here means
genuinely-wrong arguments. -}
runBuiltin : Globals -> String -> List Value -> Result String Value
runBuiltin globals name args =
    case dispatchProcessor globals name args of
        Just result ->
            result

        Nothing ->
            Err ("bad arguments to " ++ name)


dispatchProcessor : Globals -> String -> List Value -> Maybe (Result String Value)
dispatchProcessor globals name args =
    case Dict.get name nameToProcessor |> Maybe.andThen (\p -> p.run core globals name args) of
        Just result ->
            Just result

        Nothing ->
            -- Rare: the mapped processor declined (genuinely bad args). Fall back to the original
            -- scan so behaviour is identical to keying by module then trying the unqualified ones.
            firstJust (\p -> p.run core globals name args) unqualifiedProcessors



{-| Maps a function value over a list, short-circuiting on the first error. -}
mapValues : Globals -> Value -> List Value -> Result String (List Value)
mapValues globals f xs =
    case xs of
        [] ->
            Ok []

        x :: rest ->
            applyValue globals f x
                |> Result.andThen (\y -> mapValues globals f rest |> Result.map (\ys -> y :: ys))


{-| Keeps the elements for which `f` returns `True`. -}
filterValues : Globals -> Value -> List Value -> Result String (List Value)
filterValues globals f xs =
    case xs of
        [] ->
            Ok []

        x :: rest ->
            applyValue globals f x
                |> Result.andThen
                    (\keep ->
                        filterValues globals f rest
                            |> Result.map
                                (\ys ->
                                    if keep == VBool True then
                                        x :: ys

                                    else
                                        ys
                                )
                    )


{-| Left fold over values: `f x acc`, threading `acc`. -}
foldlValues : Globals -> Value -> Value -> List Value -> Result String Value
foldlValues globals f acc xs =
    case xs of
        [] ->
            Ok acc

        x :: rest ->
            applyValue globals f x
                |> Result.andThen (\g -> applyValue globals g acc)
                |> Result.andThen (\acc2 -> foldlValues globals f acc2 rest)


{-| Keeps the elements for which the predicate returns `True`, short-circuiting on error. -}
filterValues : Globals -> Value -> List Value -> Result String (List Value)
filterValues globals f xs =
    case xs of
        [] ->
            Ok []

        x :: rest ->
            applyValue globals f x
                |> Result.andThen
                    (\keep ->
                        filterValues globals f rest
                            |> Result.map
                                (\ys ->
                                    if keep == VBool True then
                                        x :: ys

                                    else
                                        ys
                                )
                    )


{-| `List.foldl`: applies `f element acc` left to right. -}
foldlValues : Globals -> Value -> Value -> List Value -> Result String Value
foldlValues globals f acc xs =
    case xs of
        [] ->
            Ok acc

        x :: rest ->
            applyValue globals f x
                |> Result.andThen (\g -> applyValue globals g acc)
                |> Result.andThen (\acc2 -> foldlValues globals f acc2 rest)



applyClosure : Globals -> List String -> Expr -> Env -> Value -> Result String Value
applyClosure globals params body closedEnv arg =
    case params of
        [] ->
            Err "cannot apply a non-function"

        p :: [] ->
            evalExpr globals (( p, arg ) :: closedEnv) body

        p :: more ->
            Ok (VClosure more body (( p, arg ) :: closedEnv))


evalCase : Globals -> Env -> Value -> List ( Pattern, Expr ) -> Result String Value
evalCase globals env subject branches =
    case branches of
        [] ->
            Err "no matching case branch"

        ( pat, body ) :: rest ->
            case matchPattern pat subject of
                Just bindings ->
                    evalExpr globals (bindings ++ env) body

                Nothing ->
                    evalCase globals env subject rest


matchPattern : Pattern -> Value -> Maybe (List ( String, Value ))
matchPattern pat value =
    case ( pat, value ) of
        ( PWild, _ ) ->
            Just []

        ( PVar name, _ ) ->
            Just [ ( name, value ) ]

        ( PInt x, VNum y ) ->
            if x == y then
                Just []

            else
                Nothing

        ( PBool x, VBool y ) ->
            if x == y then
                Just []

            else
                Nothing

        ( PStr x, VStr y ) ->
            if x == y then
                Just []

            else
                Nothing

        ( PChar x, VChar y ) ->
            if x == y then
                Just []

            else
                Nothing

        ( PNil, VList [] ) ->
            Just []

        ( PCons hp tp, VList (h :: t) ) ->
            matchPattern hp h
                |> Maybe.andThen (\hb -> matchPattern tp (VList t) |> Maybe.map (\tb -> hb ++ tb))

        ( PAlias inner name, _ ) ->
            -- `(pattern as name)` matches the inner pattern and also binds the whole value to `name`.
            matchPattern inner value
                |> Maybe.map (\binds -> ( name, value ) :: binds)

        ( PCtor name pats, VCtor vname vargs ) ->
            if name == vname && List.length pats == List.length vargs then
                matchAll pats vargs

            else
                Nothing

        ( PTup pats, VTup vs ) ->
            if List.length pats == List.length vs then
                matchAll pats vs

            else
                Nothing

        ( PRecord fields, VRecord pairs ) ->
            -- A record pattern `{ a, b }` binds each named field to its value (extra fields ignored).
            List.foldr
                (\field acc ->
                    acc
                        |> Maybe.andThen
                            (\bs ->
                                case lookupField field pairs of
                                    Just v ->
                                        Just (( field, v ) :: bs)

                                    Nothing ->
                                        Nothing
                            )
                )
                (Just [])
                fields

        _ ->
            Nothing


lookupField : String -> List ( String, Value ) -> Maybe Value
lookupField name pairs =
    case pairs of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if k == name then
                Just v

            else
                lookupField name rest


matchAll : List Pattern -> List Value -> Maybe (List ( String, Value ))
matchAll pats values =
    case ( pats, values ) of
        ( [], [] ) ->
            Just []

        ( p :: ps, v :: vs ) ->
            matchPattern p v
                |> Maybe.andThen (\b -> matchAll ps vs |> Maybe.map (\rest -> b ++ rest))

        _ ->
            Nothing


applyOp : String -> Value -> Value -> Result String Value
applyOp op a b =
    if op == "::" then
        case b of
            VList xs ->
                Ok (VList (a :: xs))

            _ ->
                Err ":: needs a list on the right"

    else if op == "++" then
        case ( a, b ) of
            ( VStr x, VStr y ) ->
                Ok (VStr (x ++ y))

            ( VList x, VList y ) ->
                Ok (VList (x ++ y))

            _ ->
                Err "++ needs two Strings or two Lists"

    else if op == "&&" || op == "||" then
        case ( a, b ) of
            ( VBool x, VBool y ) ->
                Ok (VBool (if op == "&&" then x && y else x || y))

            _ ->
                Err "&& and || need Bools"

    else if List.member op [ "==", "/=" ] then
        Ok (VBool (if op == "==" then valueEq a b else not (valueEq a b)))

    else
        case ( a, b ) of
            ( VNum x, VNum y ) ->
                arithOrCompare op x y

            ( VChar x, VChar y ) ->
                -- Chars are comparable by code point (<, <=, >, >=).
                arithOrCompare op (toFloat (Char.toCode x)) (toFloat (Char.toCode y))

            _ ->
                Err (op ++ " needs two numbers")


arithOrCompare : String -> Float -> Float -> Result String Value
arithOrCompare op x y =
    if op == "+" then
        Ok (VNum (x + y))

    else if op == "-" then
        Ok (VNum (x - y))

    else if op == "*" then
        Ok (VNum (x * y))

    else if op == "^" then
        Ok (VNum (x ^ y))

    else if op == "/" then
        -- Float division follows Elm: dividing by zero yields Infinity/NaN, it does not error.
        Ok (VNum (x / y))

    else if op == "//" then
        -- Integer division truncates toward zero; Elm defines `n // 0 == 0`.
        if y == 0 then
            Ok (VNum 0)

        else
            Ok (VNum (toFloat (truncate (x / y))))

    else if op == "<" then
        Ok (VBool (x < y))

    else if op == "<=" then
        Ok (VBool (x <= y))

    else if op == ">" then
        Ok (VBool (x > y))

    else if op == ">=" then
        Ok (VBool (x >= y))

    else
        Err ("unknown operator: " ++ op)


lookup : String -> List ( String, a ) -> Maybe a
lookup name pairs =
    case pairs of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if k == name then
                Just v

            else
                lookup name rest



-- RENDERING


renderValue : Value -> String
renderValue =
    Eval.Render.renderValue


{-| Re-exposed from Eval.Render so `Eval.htmlToString` stays available (the JS-backend test driver
calls it on a rendered view). -}
htmlToString : Value -> String
htmlToString =
    Eval.Render.htmlToString



-- PUBLIC ENTRY POINTS


{-| Evaluates a single expression in an empty scope (used for messages and the simple REPL). -}
eval : String -> String
eval src =
    case tokenize src |> Result.andThen parse |> Result.andThen (evalExpr Dict.empty []) of
        Ok v ->
            renderValue v

        Err e ->
            "Error: " ++ e


{-| Evaluates the entry expression against the top-level definitions of all files. -}
evalProject : List ( String, String ) -> String -> String
evalProject files entry =
    case parseProject files of
        Err e ->
            "Parse error: " ++ e

        Ok globals ->
            case tokenize entry |> Result.andThen parse of
                Err e ->
                    "Error: " ++ e

                Ok expr ->
                    case evalExpr globals [] expr of
                        Ok v ->
                            renderValue v

                        Err e ->
                            "Error: " ++ e


{-| Folds the message expressions through `update`, returning, per step, the message text and the
rendered model and view — the data behind the time-travel debugger. Step 0 is the initial model. -}
debugSteps : List ( String, String ) -> List String -> List String
debugSteps files messageLines =
    case parseProject files of
        Err e ->
            [ "Parse error: " ++ e ]

        Ok globals ->
            case ( evalGlobal globals "init", findDecl globals "update" ) of
                ( Ok initModel, True ) ->
                    let
                        msgs =
                            List.filter (\s -> String.trim s /= "") messageLines
                    in
                    stepFold globals initModel msgs [ formatStep globals "(init)" initModel ]

                _ ->
                    [ "Define top-level `init`, `update` and `view` to use the debugger." ]


stepFold : Globals -> Value -> List String -> List String -> List String
stepFold globals model msgs acc =
    case msgs of
        [] ->
            List.reverse acc

        line :: rest ->
            case tokenize line |> Result.andThen parse |> Result.andThen (evalExpr globals []) of
                Err e ->
                    List.reverse (("✗ " ++ line ++ " -> " ++ e) :: acc)

                Ok msg ->
                    case applyUpdate globals msg model of
                        Err e ->
                            List.reverse (("✗ " ++ line ++ " -> " ++ e) :: acc)

                        Ok next ->
                            stepFold globals next rest (formatStep globals line next :: acc)


applyUpdate : Globals -> Value -> Value -> Result String Value
applyUpdate globals msg model =
    evalExpr globals [] (Var "update")
        |> Result.andThen (\u -> applyValue globals u msg)
        |> Result.andThen (\u1 -> applyValue globals u1 model)


formatStep : Globals -> String -> Value -> String
formatStep globals label model =
    let
        viewText =
            case evalGlobal globals "view" of
                Ok _ ->
                    case evalExpr globals [] (Var "view") |> Result.andThen (\f -> applyValue globals f model) of
                        Ok v ->
                            "  view: " ++ renderValue v

                        Err _ ->
                            ""

                Err _ ->
                    ""
    in
    label ++ "  =>  model: " ++ renderValue model ++ viewText


evalGlobal : Globals -> String -> Result String Value
evalGlobal globals name =
    if findDecl globals name then
        evalExpr globals [] (Var name)

    else
        Err ("missing " ++ name)


findDecl : Globals -> String -> Bool
findDecl globals name =
    Dict.member name globals



-- LIVE APP (The Elm Architecture): drive a Browser.sandbox-style init/update/view interactively.


{-| Whether the project defines the `init`, `update` and `view` of a runnable app. -}
hasApp : List ( String, String ) -> Bool
hasApp files =
    case parseProject files of
        Ok globals ->
            findDecl globals "init" && findDecl globals "update" && findDecl globals "view"

        Err _ ->
            False


{-| The app's initial model value. For a Browser.element program `init` is `flags -> (model, cmd)`,
so it is applied to unit flags and the model taken from the tuple; for Browser.sandbox `init` is the
model directly. -}
appInit : List ( String, String ) -> Result String Value
appInit files =
    parseProject files
        |> Result.andThen
            (\globals ->
                evalGlobal globals "init"
                    |> Result.andThen
                        (\initVal ->
                            case initVal of
                                VClosure _ _ _ ->
                                    applyValue globals initVal (VTup []) |> Result.map modelOf

                                VRec _ _ _ _ ->
                                    applyValue globals initVal (VTup []) |> Result.map modelOf

                                _ ->
                                    Ok (modelOf initVal)
                        )
            )


{-| Runs `update msg model`, producing the next model value (unwrapping a Browser.element
`(model, cmd)` tuple to just the model). -}
appUpdate : List ( String, String ) -> Value -> Value -> Result String Value
appUpdate files msg model =
    parseProject files
        |> Result.andThen (\globals -> applyUpdate globals msg model |> Result.map modelOf)


{-| The model out of an init/update result: the first element of a `(model, Cmd)` tuple, else the
value itself (a Browser.sandbox model). -}
modelOf : Value -> Value
modelOf v =
    case v of
        VTup (m :: _) ->
            m

        _ ->
            v


-- Cmd/Sub-aware variants the editor uses to run effects (Random) and subscriptions (Time.every).


{-| A no-op command (e.g. `Cmd.none`, or a sandbox update with no command). -}
noCmd : Value
noCmd =
    VCtor "Cmd" []


{-| Splits an init/update result into (model, command). -}
splitMC : Value -> ( Value, Value )
splitMC v =
    case v of
        VTup (m :: c :: _) ->
            ( m, c )

        VTup (m :: _) ->
            ( m, noCmd )

        _ ->
            ( v, noCmd )


{-| Like {@link appInit} but also returns the initial command. -}
appInitCmd : List ( String, String ) -> Result String ( Value, Value )
appInitCmd files =
    parseProject files
        |> Result.andThen
            (\globals ->
                evalGlobal globals "init"
                    |> Result.andThen
                        (\initVal ->
                            case initVal of
                                VClosure _ _ _ ->
                                    applyValue globals initVal (VTup []) |> Result.map splitMC

                                VRec _ _ _ _ ->
                                    applyValue globals initVal (VTup []) |> Result.map splitMC

                                _ ->
                                    Ok ( initVal, noCmd )
                        )
            )


{-| Like {@link appUpdate} but also returns the command produced by `update`. -}
appUpdateCmd : List ( String, String ) -> Value -> Value -> Result String ( Value, Value )
appUpdateCmd files msg model =
    parseProject files |> Result.andThen (\globals -> appUpdateCmdOf globals msg model)


{-| {@link appUpdateCmd} over already-parsed globals — the editor parses once per source change and
reuses the result across a frame's update/view/subscriptions instead of re-parsing each call. -}
appUpdateCmdOf : Globals -> Value -> Value -> Result String ( Value, Value )
appUpdateCmdOf globals msg model =
    applyUpdate globals msg model |> Result.map splitMC


{-| Applies a message-producing function (a `Random.generate`/`Time.every` constructor) to a value. -}
applyMsgIn : List ( String, String ) -> Value -> Value -> Result String Value
applyMsgIn files fn arg =
    parseProject files |> Result.andThen (\globals -> applyMsgInOf globals fn arg)


{-| {@link applyMsgIn} over already-parsed globals. -}
applyMsgInOf : Globals -> Value -> Value -> Result String Value
applyMsgInOf globals fn arg =
    applyValue globals fn arg


{-| If the app subscribes via `Time.every`, the (interval-ms, toMsg) the editor wires to a tick. -}
appSubscription : List ( String, String ) -> Value -> Maybe ( Int, Value )
appSubscription files model =
    case parseProject files of
        Ok globals ->
            appSubscriptionOf globals model

        Err _ ->
            Nothing


{-| {@link appSubscription} over already-parsed globals. -}
appSubscriptionOf : Globals -> Value -> Maybe ( Int, Value )
appSubscriptionOf globals model =
    case evalGlobal globals "subscriptions" |> Result.andThen (\f -> applyValue globals f model) of
        Ok (VCtor "Sub.every" [ VNum interval, toMsg ]) ->
            Just ( round interval, toMsg )

        _ ->
            Nothing


{-| If the app subscribes (anywhere in `subscriptions`, including inside a `Sub.batch`) via
`Browser.Events.onAnimationFrameDelta`, the toMsg the editor applies to each frame's delta (in ms).
Lets animated programs — like the WebGL examples that orbit a camera over time — actually move. -}
appAnimation : List ( String, String ) -> Value -> Maybe Value
appAnimation files model =
    appSubHandler files model "Sub.animationFrame"


{-| {@link appAnimation} over already-parsed globals. -}
appAnimationOf : Globals -> Value -> Maybe Value
appAnimationOf globals model =
    appSubHandlerOf globals model "Sub.animationFrame"


{-| The handler carried by the named subscription (if the app subscribes to it, even inside a
`Sub.batch`): the `toMsg`/decoder of `Sub.animationFrame`/`Sub.keyDown`/`Sub.keyUp`/`Sub.resize`.
Lets the editor wire keyboard/resize/animation events for a Browser.element app, not just games. -}
appSubHandler : List ( String, String ) -> Value -> String -> Maybe Value
appSubHandler files model name =
    case parseProject files of
        Ok globals ->
            appSubHandlerOf globals model name

        Err _ ->
            Nothing


{-| {@link appSubHandler} over already-parsed globals. -}
appSubHandlerOf : Globals -> Value -> String -> Maybe Value
appSubHandlerOf globals model name =
    case evalGlobal globals "subscriptions" |> Result.andThen (\f -> applyValue globals f model) of
        Ok subs ->
            findSub name subs

        Err _ ->
            Nothing


{-| Searches a (possibly batched) subscription value for the first sub of constructor `name`,
returning its first argument (the handler). -}
findSub : String -> Value -> Maybe Value
findSub name v =
    case v of
        VCtor n args ->
            if n == name then
                List.head args

            else
                firstJust (findSub name) args

        VList items ->
            firstJust (findSub name) items

        _ ->
            Nothing


{-| Runs an event decoder (e.g. a `Browser.Events.onKeyDown` decoder) against a JSON event string
the editor constructs (like `{"key":"w"}`), yielding the message to dispatch. -}
runEventDecoder : List ( String, String ) -> Value -> String -> Result String Value
runEventDecoder files decoder jsonText =
    parseProject files
        |> Result.andThen
            (\globals ->
                Eval.Json.parseJson jsonText |> Result.andThen (\json -> Eval.Json.runDecoder applyValue globals decoder json)
            )


firstJust : (a -> Maybe b) -> List a -> Maybe b
firstJust f xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            case f x of
                Just y ->
                    Just y

                Nothing ->
                    firstJust f rest


{-| Resolves a `Random.generate` command: samples its generator with the editor's `seed` and applies
the message constructor, yielding the message to dispatch and the next seed. -}
randomCmd : List ( String, String ) -> Int -> Value -> Maybe ( Value, Int )
randomCmd files seed cmd =
    case cmd of
        VCtor "Cmd.random" [ toMsg, gen ] ->
            case parseProject files of
                Ok globals ->
                    let
                        ( v, seed2 ) =
                            sampleGen globals seed gen
                    in
                    case applyMsgIn files toMsg v of
                        Ok msg ->
                            Just ( msg, seed2 )

                        Err _ ->
                            Nothing

                Err _ ->
                    Nothing

        _ ->
            Nothing


{-| If the command is an `Http.get`, the (url, expect) the editor needs to issue a real request and
build the response message. The `expect` carries the message constructor (and, for JSON, a decoder). -}
httpCmd : Value -> Maybe ( String, Value )
httpCmd cmd =
    case cmd of
        VCtor "Cmd.http" [ VStr url, expect ] ->
            Just ( url, expect )

        _ ->
            Nothing


{-| If the command is a `File.Select.file`, the message constructor to apply to the chosen file; the
editor opens a real browser file picker and feeds the result back through {@link fileSelected}. -}
fileSelectCmd : Value -> Maybe ( Value, Bool )
fileSelectCmd cmd =
    case cmd of
        VCtor "Cmd.fileSelect" [ toMsg ] ->
            Just ( toMsg, False )

        VCtor "Cmd.fileSelectMany" [ toMsg ] ->
            -- `File.Select.files`: toMsg is `File -> List File -> msg`.
            Just ( toMsg, True )

        _ ->
            Nothing


{-| The message to dispatch once the user picks a file: `toMsg` applied to a `File` value carrying
the file's name and text content (so `File.name`/`File.toString` work on it). When `many` (from
`File.Select.files`), toMsg is `File -> List File -> msg`, so it's also applied to the rest of the
selection (empty — the editor's picker yields one file). -}
fileSelected : List ( String, String ) -> Value -> Bool -> String -> String -> Result String Value
fileSelected files toMsg many name content =
    let
        file =
            VCtor "File" [ VStr name, VStr content ]
    in
    if many then
        applyMsgIn files toMsg file
            |> Result.andThen (\partial -> applyMsgIn files partial (VList []))

    else
        applyMsgIn files toMsg file


{-| Resolves a `Task.perform` command (over an already-evaluated `Task.value`, e.g. from
`File.toString`) to the message to dispatch — so a script's `Task.perform GotContent (File.toString
file)` delivers the content. Returns `Nothing` for other commands. -}
taskResult : List ( String, String ) -> Value -> Maybe (Result String Value)
taskResult files cmd =
    case cmd of
        VCtor "Cmd.task" [ toMsg, task ] ->
            -- Task.perform: apply toMsg to the task's resolved value.
            Maybe.map (\v -> applyMsgIn files toMsg v) (taskValueOf task)

        VCtor "Cmd.taskAttempt" [ toMsg, task ] ->
            -- Task.attempt: apply toMsg to `Ok value` (the editor's tasks never fail).
            Maybe.map (\v -> applyMsgIn files toMsg (VCtor "Ok" [ v ])) (taskValueOf task)

        _ ->
            Nothing


{-| Resolves the opaque tasks the editor knows how to run to their success value: a held `Task.value`
(e.g. File.toString), the browser viewport (Browser.Dom.getViewport, with a sensible fixed size), or
a WebGL texture load (kept as its url-carrying value so the GL bridge can load the image). -}
taskValueOf : Value -> Maybe Value
taskValueOf task =
    case task of
        VCtor "Task.value" [ v ] ->
            Just v

        VBuiltin "Dom.getViewport" _ ->
            Just viewportValue

        VCtor "Dom.getViewport" _ ->
            Just viewportValue

        VBuiltin "Dom.getViewportOf" _ ->
            Just viewportValue

        VCtor "Dom.getViewportOf" _ ->
            Just viewportValue

        VBuiltin "Dom.setViewportOf" _ ->
            Just (VTup [])

        VCtor "Dom.setViewportOf" _ ->
            Just (VTup [])

        VBuiltin "Texture.load" args ->
            Just (VCtor "Texture.load" args)

        VCtor "Texture.load" args ->
            Just (VCtor "Texture.load" args)

        VBuiltin "WebGL.Texture.load" args ->
            Just (VCtor "Texture.load" args)

        VCtor "WebGL.Texture.load" args ->
            Just (VCtor "Texture.load" args)

        -- A fully-applied `Texture.loadWith options url` evaluates to a `VCtor` (not a `VBuiltin`), so
        -- the VBuiltin cases never matched it — `Thwomp`'s textures never resolved and it stuck on
        -- "Loading textures...". Drop the options record, keep the url for the GL bridge.
        VBuiltin "Texture.loadWith" args ->
            Just (VCtor "Texture.load" (List.drop 1 args))

        VCtor "Texture.loadWith" args ->
            Just (VCtor "Texture.load" (List.drop 1 args))

        VCtor "WebGL.Texture.loadWith" args ->
            Just (VCtor "Texture.load" (List.drop 1 args))

        _ ->
            Nothing


{-| The editor's stand-in for `Browser.Dom.getViewport`: a viewport record at a fixed preview size
(the interpreter can't read the real DOM size), enough for size-driven programs like Thwomp to run. -}
viewportValue : Value
viewportValue =
    let
        box =
            VRecord [ ( "x", VNum 0 ), ( "y", VNum 0 ), ( "width", VNum 500 ), ( "height", VNum 500 ) ]

        size =
            VRecord [ ( "width", VNum 500 ), ( "height", VNum 500 ) ]
    in
    VRecord [ ( "scene", size ), ( "viewport", box ) ]


{-| Builds the message to dispatch when an HTTP request finishes. For `expectString` it is
`toMsg (Ok body)`; for `expectJson` the body is parsed and run through the decoder, giving
`toMsg (Ok value)` (or an `Err` on a network/decode failure). -}
httpResult : List ( String, String ) -> Value -> Maybe String -> Result String Value
httpResult files expect body =
    parseProject files |> Result.andThen (\globals -> httpResultIn globals expect body)


httpResultIn : Globals -> Value -> Maybe String -> Result String Value
httpResultIn globals expect body =
    case expect of
        VCtor "Http.expect" [ toMsg ] ->
            applyValue globals toMsg (okOrErr body)

        VCtor "Http.expectJson" [ toMsg, decoder ] ->
            case body of
                Just text ->
                    case Eval.Json.parseJson text |> Result.andThen (\json -> Eval.Json.runDecoder applyValue globals decoder json) of
                        Ok v ->
                            applyValue globals toMsg (VCtor "Ok" [ v ])

                        Err _ ->
                            applyValue globals toMsg (VCtor "Err" [ VCtor "BadBody" [] ])

                Nothing ->
                    applyValue globals toMsg (VCtor "Err" [ VCtor "NetworkError" [] ])

        _ ->
            Err "unknown Http expect"


okOrErr : Maybe String -> Value
okOrErr body =
    case body of
        Just text ->
            VCtor "Ok" [ VStr text ]

        Nothing ->
            VCtor "Err" [ VCtor "NetworkError" [] ]


{-| Samples a generator with a linear-congruential step of the seed, returning (value, next seed). -}
sampleGen : Globals -> Int -> Value -> ( Value, Int )
sampleGen globals seed gen =
    let
        s =
            abs (modBy 2147483647 (seed * 1103515245 + 12345))
    in
    case gen of
        VCtor "Random.Gen" [ VStr "int", VNum lo, VNum hi ] ->
            ( VNum (toFloat (round lo + modBy (round hi - round lo + 1) s)), s )

        VCtor "Random.Gen" [ VStr "float", VNum lo, VNum hi ] ->
            ( VNum (lo + (hi - lo) * (toFloat s / 2147483647)), s )

        VCtor "Random.Gen" [ VStr "uniform", VList xs ] ->
            ( listGet (modBy (max 1 (List.length xs)) s) xs, s )

        VCtor "Random.Gen" [ VStr "constant", x ] ->
            ( x, seed )

        VCtor "Random.Gen" [ VStr "map", f, g ] ->
            let
                ( v, s2 ) =
                    sampleGen globals seed g
            in
            ( applyVal globals f v, s2 )

        VCtor "Random.Gen" [ VStr "map2", f, g1, g2 ] ->
            let
                ( v1, s1 ) =
                    sampleGen globals seed g1

                ( v2, s2 ) =
                    sampleGen globals s1 g2
            in
            ( applyVal globals (applyVal globals f v1) v2, s2 )

        VCtor "Random.Gen" [ VStr "map3", f, g1, g2, g3 ] ->
            let
                ( v1, s1 ) =
                    sampleGen globals seed g1

                ( v2, s2 ) =
                    sampleGen globals s1 g2

                ( v3, s3 ) =
                    sampleGen globals s2 g3
            in
            ( applyVal globals (applyVal globals (applyVal globals f v1) v2) v3, s3 )

        VCtor "Random.Gen" [ VStr "pair", g1, g2 ] ->
            let
                ( v1, s1 ) =
                    sampleGen globals seed g1

                ( v2, s2 ) =
                    sampleGen globals s1 g2
            in
            ( VTup [ v1, v2 ], s2 )

        VCtor "Random.Gen" [ VStr "list", VNum n, g ] ->
            sampleList globals seed g (round n) []

        VCtor "Random.Gen" [ VStr "andThen", f, g ] ->
            let
                ( v, s2 ) =
                    sampleGen globals seed g
            in
            sampleGen globals s2 (applyVal globals f v)

        _ ->
            ( VNum 0, s )


{-| Applies a function value to one argument, falling back to a number on error (so generator
sampling stays total). -}
applyVal : Globals -> Value -> Value -> Value
applyVal globals f a =
    Result.withDefault (VNum 0) (applyValue globals f a)


{-| Samples a `Random.list n gen` by drawing `n` values, threading the seed. -}
sampleList : Globals -> Int -> Value -> Int -> List Value -> ( Value, Int )
sampleList globals seed g n acc =
    if n <= 0 then
        ( VList (List.reverse acc), seed )

    else
        let
            ( v, s2 ) =
                sampleGen globals seed g
        in
        sampleList globals s2 g (n - 1) (v :: acc)


listGet : Int -> List Value -> Value
listGet n xs =
    case xs of
        [] ->
            VCtor "Nothing" []

        x :: rest ->
            if n <= 0 then
                x

            else
                listGet (n - 1) rest


{-| Evaluates `view model` to the Html `Value` tree the editor renders to live Html. -}
appView : List ( String, String ) -> Value -> Result String Value
appView files model =
    parseProject files |> Result.andThen (\globals -> appViewOf globals model)


{-| {@link appView} over already-parsed globals. -}
appViewOf : Globals -> Value -> Result String Value
appViewOf globals model =
    evalExpr globals [] (Var "view")
        |> Result.andThen (\f -> applyValue globals f model)


{-| Evaluates the project's `main` to a value (e.g. a static Html tree, a Browser.sandbox config, or
a plain value) — what the editor renders for the selected file. -}
mainValue : List ( String, String ) -> Result String Value
mainValue files =
    parseProject files |> Result.andThen (\globals -> evalExpr globals [] (Var "main"))


{-| Applies an event handler (e.g. an `onInput` message constructor) to the event's string payload,
producing the message value to dispatch. -}
applyHandler : List ( String, String ) -> Value -> String -> Result String Value
applyHandler files handler payload =
    parseProject files
        |> Result.andThen (\globals -> applyValue globals handler (VStr payload))


{-| Headless render of a single-file app's initial view to an HTML string (used in tests and as a
quick non-DOM preview): runs `init` then `view`, serialising the Html `Value` tree. -}
renderProgram : String -> String
renderProgram source =
    let
        files =
            [ ( "Main.elm", source ) ]
    in
    if hasApp files then
        -- A Browser.sandbox-style app: render the initial view (init |> view).
        case appInit files |> Result.andThen (appView files) of
            Ok html ->
                htmlToString html

            Err e ->
                "app error: " ++ e

    else
        -- A static program: render `main` (a Html value or a plain value) directly.
        case mainValue files of
            Ok (VCtor "Playground.game" [ _, _, mem ]) ->
                -- A `game`: draw its initial frame (no keys, time 0).
                case gameView files [] 0 mem of
                    Ok html ->
                        htmlToString html

                    Err e ->
                        "game error: " ++ e

            Ok (VCtor "Playground.animation" [ _ ]) ->
                -- An `animation`: draw its initial frame (time 0).
                case gameView files [] 0 (VCtor "$Anim" []) of
                    Ok html ->
                        htmlToString html

                    Err e ->
                        "animation error: " ++ e

            Ok v ->
                htmlToString v

            Err e ->
                "main error: " ++ e


-- PLAYGROUND game loop: thin wrappers that inject the evaluator (mainValue/applyValue) into
-- Eval.Playground and re-expose the game functions on Eval's public surface. The pure Playground
-- helpers (Eval.Playground.runPlayground/mkShape/playgroundColor/…) are called qualified at their
-- use sites; only these game functions need a local definition (for the injection + re-export).


gameInitMem : List ( String, String ) -> Maybe Value
gameInitMem files =
    Eval.Playground.gameInitMem mainValue files


gameView : List ( String, String ) -> List String -> Float -> Value -> Result String Value
gameView files keys time mem =
    Eval.Playground.gameView mainValue applyValue files keys time mem


gameStep : List ( String, String ) -> List String -> Float -> Value -> Result String Value
gameStep files keys time mem =
    Eval.Playground.gameStep mainValue applyValue files keys time mem
