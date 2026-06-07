module Eval exposing (eval, evalProject, debugSteps, lookup, renderValue, appInit, appUpdate, appView, hasApp, renderProgram, mainValue, applyHandler, appInitCmd, appUpdateCmd, appSubscription, appAnimation, appSubHandler, runEventDecoder, randomCmd, applyMsgIn, gameInitMem, gameView, gameStep, httpCmd, httpResult, fileSelectCmd, fileSelected, taskResult, appUpdateCmdOf, appViewOf, appSubscriptionOf, appAnimationOf, appSubHandlerOf, applyMsgInOf)

{-| The evaluator for the interpreted language. Global (top-level) definitions are threaded through
evaluation so all definitions across the project's files form one mutually-recursive scope. Public
entry points: `eval` (one expression), `evalProject` (entry expression against all files) and
`debugSteps` (fold messages through update for the time-travel debugger). -}

import Bitwise
import Dict
import EvalBitwise
import EvalChar
import EvalCore exposing (Core, Processor, asList, asNum, keepJust, maybeValue, valueEq)
import EvalDebug
import EvalJson
import EvalPlayground
import EvalRender
import EvalString
import EvalTuple
import Lang exposing (Decl, Env, Expr(..), Globals, Pattern(..), Value(..))
import Lexer exposing (tokenize)
import Parser exposing (parse, parseProject)
import Set exposing (Set)


{-| Native builtins available to interpreted programs (resolved when a name is in neither the local
scope nor the project's top-level definitions), as a `Set` so membership is O(1) — it is checked on
every name that isn't a local/global, including in hot interpreter loops. Includes Html
element/attribute constructors so TEA programs (Browser.sandbox apps) can be rendered live. -}
builtins : Set String
builtins =
    Set.fromList builtinNames


builtinNames : List String
builtinNames =
    htmlTags
        ++ htmlStringAttrs
        ++ htmlBoolAttrs
        ++ [ "text", "onClick", "onInput", "on", "preventDefaultOn", "stopPropagationOn", "style", "toString", "negate", "not" ]
        ++ [ "Browser.sandbox", "Browser.element" ]
        ++ [ "List.range", "List.map", "List.length", "List.sum", "Maybe.withDefault" ]
        ++ [ "List.reverse", "List.head", "List.tail", "List.isEmpty", "List.maximum", "List.minimum", "List.sort", "List.concat", "List.product" ]
        ++ [ "List.filter", "List.append", "List.member", "List.filterMap", "List.take", "List.drop", "List.any", "List.all", "List.indexedMap", "List.repeat", "List.sortBy", "List.foldl", "List.foldr", "List.map2", "List.concatMap" ]
        ++ [ "Maybe.map", "Maybe.andThen", "Maybe.map2", "Maybe.map3", "Maybe.map4", "Maybe.map5", "Result.withDefault", "Result.map", "Result.map2", "Result.map3", "Result.map4", "Result.map5", "Result.andThen", "Result.toMaybe", "Result.mapError", "Result.fromMaybe" ]
        ++ [ "identity", "always", "min", "max", "modBy", "remainderBy", "clamp", "xor" ]
        ++ [ "List.partition", "List.intersperse", "List.unzip", "List.map3", "List.map4", "List.map5", "List.sortWith", "compare", "List.singleton" ]
        ++ processorNames
        -- Html.Lazy / Svg.Lazy: the interpreter re-renders every frame, so `lazy` just forces (applies
        -- the view function); both the qualified and `exposing (lazy, …)` forms are accepted.
        ++ [ "lazy", "lazy2", "lazy3", "lazy4", "lazy5" ]
        ++ [ "Html.Lazy.lazy", "Html.Lazy.lazy2", "Html.Lazy.lazy3", "Html.Lazy.lazy4", "Html.Lazy.lazy5" ]
        ++ [ "Svg.Lazy.lazy", "Svg.Lazy.lazy2", "Svg.Lazy.lazy3", "Svg.Lazy.lazy4", "Svg.Lazy.lazy5" ]
        ++ [ "Dict.empty", "Dict.singleton", "Dict.fromList", "Dict.toList", "Dict.get", "Dict.insert", "Dict.remove", "Dict.member", "Dict.size", "Dict.isEmpty", "Dict.keys", "Dict.values", "Dict.map", "Dict.filter", "Dict.foldl", "Dict.foldr", "Dict.partition", "Dict.union", "Dict.diff", "Dict.intersect", "Dict.update" ]
        ++ [ "Set.empty", "Set.singleton", "Set.fromList", "Set.toList", "Set.insert", "Set.remove", "Set.member", "Set.size", "Set.isEmpty", "Set.union", "Set.diff", "Set.intersect", "Set.foldl", "Set.foldr", "Set.map", "Set.filter", "Set.partition" ]
        ++ [ "Array.empty", "Array.initialize", "Array.repeat", "Array.fromList", "Array.toList", "Array.toIndexedList", "Array.get", "Array.set", "Array.push", "Array.append", "Array.length", "Array.isEmpty", "Array.slice", "Array.map", "Array.indexedMap", "Array.foldl", "Array.foldr", "Array.filter" ]
        ++ [ "cos", "sin", "tan", "sqrt", "toFloat", "round", "floor", "ceiling", "truncate", "abs" ]
        ++ [ "asin", "acos", "atan", "atan2", "logBase", "radians", "turns", "isNaN", "isInfinite" ]
        ++ [ "Time.millisToPosix", "Time.posixToMillis", "Time.toHour", "Time.toMinute", "Time.toSecond", "Time.every" ]
        ++ [ "Random.int", "Random.float", "Random.uniform", "Random.generate" ]
        ++ [ "Random.map", "Random.map2", "Random.map3", "Random.pair", "Random.list", "Random.constant", "Random.andThen" ]
        ++ [ "Http.get", "Http.expectString", "Http.expectJson" ]
        ++ [ "File.Select.file", "File.Select.files", "File.toString", "File.toUrl", "File.name", "File.mime", "File.size", "Task.perform", "Task.attempt" ]
        ++ [ "field", "at", "map", "oneOrMore", "map2", "map3", "map4", "map5", "map6", "map7", "map8", "succeed", "list", "andThen", "oneOf", "nullable" ]
        ++ [ "Encode.string", "Encode.int", "Encode.float", "Encode.bool", "Encode.object", "Encode.list", "Encode.encode" ]
        ++ webglNames
        ++ playgroundNames


{-| elm-explorations/webgl + linear-algebra builtins. The editor interpreter evaluates these to
opaque values (and `WebGL.toHtml` to a canvas preview that reports the scene's entity count) so WebGL
programs run without errors; the JS backend does the real GPU rendering in the browser. -}
webglNames : List String
webglNames =
    [ "WebGL.toHtml", "WebGL.toHtmlWith", "WebGL.entity", "WebGL.entityWith" ]
        ++ [ "WebGL.triangles", "WebGL.indexedTriangles", "WebGL.lines", "WebGL.lineStrip", "WebGL.lineLoop", "WebGL.points", "WebGL.triangleStrip", "WebGL.triangleFan" ]
        ++ [ "WebGL.clearColor", "WebGL.depth", "WebGL.alpha", "WebGL.antialias", "WebGL.Texture.load", "WebGL.Texture.size" ]
        ++ [ "vec2", "vec3", "vec4" ]
        ++ [ "Mat4.makePerspective", "Mat4.makeLookAt", "Mat4.makeRotate", "Mat4.makeTranslate", "Mat4.makeScale", "Mat4.mul", "Mat4.mulAffine", "Mat4.transform", "Mat4.inverse", "Mat4.transpose", "Mat4.makeOrtho2D" ]
        -- The conventional `Math.Vector3 as Vec3` / `Math.Vector2 as Vec2` aliases. Opaque to the
        -- interpreter; the JS WebGL bridge computes them for real (see $glScalar in dom.js).
        ++ [ "Vec3.add", "Vec3.sub", "Vec3.scale", "Vec3.normalize", "Vec3.negate", "Vec3.dot", "Vec3.cross", "Vec3.length", "Vec3.distance", "Vec3.direction", "Vec3.getX", "Vec3.getY", "Vec3.getZ", "Vec3.setX", "Vec3.setY", "Vec3.setZ", "Vec3.i", "Vec3.j", "Vec3.k", "Vec3.fromRecord", "Vec3.toRecord" ]
        ++ [ "Vec2.add", "Vec2.sub", "Vec2.scale", "Vec2.normalize", "Vec2.length", "Vec2.getX", "Vec2.getY" ]
        -- `WebGL.Texture as Texture` aliased names, plus the texture-option constants.
        ++ [ "Texture.load", "Texture.loadWith", "Texture.size", "Texture.nearest", "Texture.linear", "Texture.repeat", "Texture.clampToEdge", "Texture.mirroredRepeat", "Texture.nearestMipmapNearest", "Texture.linearMipmapLinear" ]
        -- `Browser.Dom as Dom`: getViewport/getViewportOf are opaque Tasks (fed to Task.perform);
        -- setViewportOf scrolls a container (a unit Task — a no-op under the headless interpreter).
        ++ [ "Dom.getViewport", "Dom.getViewportOf", "Dom.setViewportOf" ]


{-| evancz/elm-playground builtins: shape constructors, transforms, colours and the `picture`/
`animation` entry points, implemented natively in the editor (rendering to SVG `Value` trees). The
`circle`/`polygon` names overlap with the Svg builtins but are disambiguated at run time by the
argument types (`circle color radius` vs `circle attrs children`). -}
playgroundNames : List String
playgroundNames =
    [ "picture", "animation", "game", "oval", "rectangle", "square", "triangle", "pentagon", "hexagon", "octagon", "words", "image" ]
        ++ [ "move", "moveUp", "moveDown", "moveLeft", "moveRight", "moveX", "moveY", "rotate", "scale", "fade" ]
        ++ [ "rgb", "spin", "wave", "zigzag", "toX", "toY", "degrees" ]


{-| `Browser.Events` subscription functions, recognised by their (unqualified) field name so the
import alias (`as E`, `as Events`, …) doesn't matter. `onAnimationFrameDelta` is driven live by the
editor; the rest are accepted as opaque (no-op) subscriptions so the programs run. -}
browserEventSubs : List String
browserEventSubs =
    [ "onAnimationFrameDelta", "onAnimationFrame", "onResize", "onMouseMove", "onMouseDown", "onMouseUp", "onKeyDown", "onKeyUp", "onKeyPress", "onVisibilityChange" ]


{-| Set versions of the builtin categories `runBuiltin` dispatches on, so the membership tests it
runs on every builtin call (before the main `case`) are O(1) instead of a linear scan. -}
playgroundSet : Set String
playgroundSet =
    Set.fromList playgroundNames


webglSet : Set String
webglSet =
    Set.fromList webglNames


vec3OpsSet : Set String
vec3OpsSet =
    Set.fromList vec3Ops


browserEventSet : Set String
browserEventSet =
    Set.fromList browserEventSubs


{-| `Json.Decode` function names, recognised after an import alias (`as D`, `as Decode`, …) so
`D.succeed`, `Decode.at`, … resolve to the bare decoder builtin regardless of the alias. -}
jsonDecodeNames : List String
jsonDecodeNames =
    [ "succeed", "map", "map2", "map3", "map4", "map5", "map6", "map7", "map8", "field", "at", "list", "oneOf", "oneOrMore", "andThen", "nullable", "string", "int", "float", "bool" ]


{-| The Html (and inline SVG) element builtins (each takes a list of attributes then a list of
children). Inline SVG renders directly in the browser, so `svg`/`circle`/… serialize like any node. -}
htmlTags : List String
htmlTags =
    [ "div", "button", "p", "span", "h1", "h2", "h3", "h4", "h5", "h6", "ul", "ol", "li", "pre", "code", "input", "textarea", "select", "option", "label", "a", "section", "strong", "em", "br", "img", "table", "tr", "td", "th", "blockquote", "cite", "hr", "nav", "header", "footer" ]
        ++ [ "svg", "circle", "rect", "line", "ellipse", "polygon", "polyline", "path", "g", "text_", "defs", "stop", "linearGradient", "radialGradient" ]


{-| `Html.Attributes` / `Svg.Attributes` taking a single string, rendered as `key=value`. (`type_`
maps to `type`; the camelCase SVG names map to their hyphenated attribute — see `attrKey`.) -}
htmlStringAttrs : List String
htmlStringAttrs =
    [ "placeholder", "value", "type_", "class", "id", "href", "src", "title", "alt", "name", "for", "target", "rel", "width", "height", "rows", "cols", "autocomplete", "step" ]
        ++ [ "viewBox", "cx", "cy", "r", "x", "y", "x1", "y1", "x2", "y2", "rx", "ry", "fill", "stroke", "points", "d", "transform", "offset", "opacity" ]
        ++ [ "strokeWidth", "strokeLinecap", "strokeDasharray", "fillOpacity", "stopColor", "textAnchor", "fontSize", "fontFamily", "gradientUnits" ]


{-| `Html.Attributes` taking a single bool, rendered as a bare `key` when `True`. -}
htmlBoolAttrs : List String
htmlBoolAttrs =
    [ "checked", "disabled", "selected", "readonly", "autofocus", "hidden", "multiple" ]


{-| How many arguments a builtin consumes before it runs — an O(1) lookup (built once), since it is
queried on every argument applied to every builtin. The default (a name not in the table) is 2. -}
arity : String -> Int
arity name =
    Dict.get name arityTable |> Maybe.withDefault 2


arityTable : Dict String Int
arityTable =
    [ ( 0, [ "Dict.empty", "Set.empty", "Array.empty" ] )
    , ( 1
      , [ "text", "onClick", "onInput", "toString", "negate", "not", "Browser.sandbox", "Browser.element", "List.length", "List.sum" ]
            ++ [ "List.reverse", "List.head", "List.tail", "List.isEmpty", "List.maximum", "List.minimum", "List.sort", "List.concat", "List.product", "List.singleton", "identity", "Result.toMaybe" ]
            ++ [ "File.toString", "File.toUrl", "File.name", "File.mime", "File.size" ]
            ++ [ "Dict.fromList", "Dict.toList", "Dict.keys", "Dict.values", "Dict.size", "Dict.isEmpty", "List.unzip", "Set.fromList", "Set.toList", "Set.size", "Set.isEmpty", "Set.singleton", "Array.fromList", "Array.toList", "Array.toIndexedList", "Array.length", "Array.isEmpty" ]
            ++ [ "WebGL.triangles", "WebGL.lines", "WebGL.lineStrip", "WebGL.lineLoop", "WebGL.points", "WebGL.triangleStrip", "WebGL.triangleFan", "WebGL.depth", "WebGL.alpha", "WebGL.Texture.load", "WebGL.Texture.size", "Mat4.makeTranslate", "Mat4.makeScale", "Mat4.inverse", "Mat4.transpose" ]
            ++ [ "Vec3.normalize", "Vec3.negate", "Vec3.length", "Vec3.getX", "Vec3.getY", "Vec3.getZ", "Vec3.fromRecord", "Vec3.toRecord", "Vec2.normalize", "Vec2.length", "Vec2.getX", "Vec2.getY", "Texture.load", "Texture.size" ]
            ++ browserEventSubs
            ++ [ "cos", "sin", "tan", "sqrt", "toFloat", "round", "floor", "ceiling", "truncate", "abs", "asin", "acos", "atan", "radians", "turns", "isNaN", "isInfinite", "Time.millisToPosix", "Time.posixToMillis", "picture", "animation", "Http.get", "Http.expectString", "succeed", "list", "oneOf", "nullable", "Encode.string", "Encode.int", "Encode.float", "Encode.bool", "Encode.object" ]
            ++ [ "toX", "toY", "degrees", "Random.constant" ]
            ++ htmlStringAttrs
            ++ htmlBoolAttrs
      )
    , ( 3
      , [ "Dict.insert", "Dict.foldl", "Dict.foldr", "Dict.update", "Set.foldl", "Set.foldr", "Array.foldl", "Array.foldr", "Array.set", "Array.slice" ]
            ++ [ "Result.map2", "List.foldl", "List.foldr", "List.map2", "clamp", "Maybe.map2" ]
            ++ [ "WebGL.toHtmlWith", "vec3", "Mat4.makeLookAt" ]
            ++ [ "oval", "rectangle", "move", "rgb", "game", "image", "map2", "Random.map2" ]
            ++ [ "lazy2", "Html.Lazy.lazy2", "Svg.Lazy.lazy2" ]
      )
    , ( 4
      , [ "List.map3", "Maybe.map3", "Result.map3" ]
            ++ [ "WebGL.entity", "vec4", "WebGL.clearColor", "Mat4.makePerspective", "Mat4.makeOrtho2D" ]
            ++ [ "wave", "zigzag", "map3", "Random.map3" ]
            ++ [ "lazy3", "Html.Lazy.lazy3", "Svg.Lazy.lazy3" ]
      )
    , ( 5
      , [ "List.map4", "Maybe.map4", "Result.map4", "WebGL.entityWith", "map4" ]
            ++ [ "lazy4", "Html.Lazy.lazy4", "Svg.Lazy.lazy4" ]
      )
    , ( 6
      , [ "List.map5", "Maybe.map5", "Result.map5", "map5" ]
            ++ [ "lazy5", "Html.Lazy.lazy5", "Svg.Lazy.lazy5" ]
      )
    , ( 7, [ "map6" ] )
    , ( 8, [ "map7" ] )
    , ( 9, [ "map8" ] )
    ]
        ++ processorArities
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
                                case EvalPlayground.playgroundColor name of
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


pairKey : Value -> Maybe Value
pairKey p =
    case p of
        VTup [ k, _ ] ->
            Just k

        _ ->
            Nothing


pairValue : Value -> Maybe Value
pairValue p =
    case p of
        VTup [ _, v ] ->
            Just v

        _ ->
            Nothing


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


{-| Insert/replace, keeping keys unique and preserving insertion order (new entry appended). -}
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


mapDict : Globals -> Value -> List Value -> Result String Value
mapDict globals f pairs =
    case pairs of
        [] ->
            Ok (mkDict [])

        (VTup [ k, v ]) :: rest ->
            applyValue globals f k
                |> Result.andThen (\g -> applyValue globals g v)
                |> Result.andThen
                    (\v2 -> mapDict globals f rest |> Result.map (\d -> mkDict (VTup [ k, v2 ] :: dictPairs d)))

        _ :: rest ->
            mapDict globals f rest


filterDict : Globals -> Value -> List Value -> Result String Value
filterDict globals f pairs =
    case pairs of
        [] ->
            Ok (mkDict [])

        ((VTup [ k, v ]) as p) :: rest ->
            applyValue globals f k
                |> Result.andThen (\g -> applyValue globals g v)
                |> Result.andThen
                    (\keep ->
                        filterDict globals f rest
                            |> Result.map
                                (\d ->
                                    if keep == VBool True then
                                        mkDict (p :: dictPairs d)

                                    else
                                        d
                                )
                    )

        _ :: rest ->
            filterDict globals f rest


{-| `Dict.partition`: splits pairs into (predicate holds, predicate fails) as two Dicts. -}
partitionDict : Globals -> Value -> List Value -> Result String Value
partitionDict globals f pairs =
    case pairs of
        [] ->
            Ok (VTup [ mkDict [], mkDict [] ])

        ((VTup [ k, v ]) as p) :: rest ->
            applyValue globals f k
                |> Result.andThen (\g -> applyValue globals g v)
                |> Result.andThen
                    (\keep ->
                        partitionDict globals f rest
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
            partitionDict globals f rest


foldlDict : Globals -> Value -> Value -> List Value -> Result String Value
foldlDict globals f acc pairs =
    case pairs of
        [] ->
            Ok acc

        (VTup [ k, v ]) :: rest ->
            applyValue globals f k
                |> Result.andThen (\g -> applyValue globals g v)
                |> Result.andThen (\h -> applyValue globals h acc)
                |> Result.andThen (\acc2 -> foldlDict globals f acc2 rest)

        _ :: rest ->
            foldlDict globals f acc rest


-- SET (a list of unique values, wrapped as `VCtor "Set" [ VList elems ]`) ------------------------


{-| A Set keeps its elements sorted (like elm/core's tree-backed Set), so `toList`/`foldl` visit
them in ascending order regardless of insertion order. -}
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


{-| Append `x` only if no equal element is already present (sets keep unique values, insertion order). -}
setInsert : Value -> List Value -> List Value
setInsert x xs =
    if List.any (valueEq x) xs then
        xs

    else
        xs ++ [ x ]


{-| Applies the curried function `f` to each argument in turn (left to right). -}
applyAllValues : Globals -> Value -> List Value -> Result String Value
applyAllValues globals f args =
    List.foldl (\a acc -> acc |> Result.andThen (\g -> applyValue globals g a)) (Ok f) args


{-| `List.mapN`: zips several lists, applying `f` to one element from each, stopping at the shortest.
(Generalises `List.map3` to any arity, avoiding 4-/5-tuple patterns that Elm forbids.) -}
mapNValues : Globals -> Value -> List (List Value) -> Result String (List Value)
mapNValues globals f lists =
    if List.isEmpty lists || List.any List.isEmpty lists then
        Ok []

    else
        applyAllValues globals f (List.filterMap List.head lists)
            |> Result.andThen
                (\v ->
                    mapNValues globals f (List.map (List.drop 1) lists)
                        |> Result.map (\vs -> v :: vs)
                )


{-| The `Order` custom-type value for a Java/host `Order`. -}
orderValue : Order -> Value
orderValue o =
    case o of
        LT ->
            VCtor "LT" []

        EQ ->
            VCtor "EQ" []

        GT ->
            VCtor "GT" []


{-| Runs a user comparator `f a b` and reads its `Order` result (defaulting to `EQ` on error). -}
orderFromCompare : Globals -> Value -> Value -> Value -> Order
orderFromCompare globals f a b =
    case applyValue globals f a |> Result.andThen (\g -> applyValue globals g b) of
        Ok (VCtor "LT" _) ->
            LT

        Ok (VCtor "GT" _) ->
            GT

        _ ->
            EQ


{-| `Maybe.mapN`: if every argument is `Just`, apply `f` to the unwrapped values; otherwise
`Nothing`. (Elm tuples max out at three elements, so this avoids a 4-/5-tuple pattern.) -}
maybeMapN : Globals -> Value -> List Value -> Result String Value
maybeMapN globals f margs =
    case allJust margs of
        Just xs ->
            List.foldl (\x acc -> acc |> Result.andThen (\g -> applyValue globals g x)) (Ok f) xs
                |> Result.map (\y -> VCtor "Just" [ y ])

        Nothing ->
            Ok (VCtor "Nothing" [])


{-| `Result.mapN`: if every argument is `Ok`, apply `f` to the unwrapped values; otherwise the first
`Err`. (Avoids 4-/5-tuple patterns, which Elm forbids.) -}
resultMapN : Globals -> Value -> List Value -> Result String Value
resultMapN globals f rs =
    case allOk rs of
        Ok xs ->
            applyAllValues globals f xs |> Result.map (\y -> VCtor "Ok" [ y ])

        Err e ->
            Ok (VCtor "Err" [ e ])


{-| The unwrapped values if every element is `Ok _`, else the first wrapped `Err` value. -}
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


{-| The unwrapped values if every element is `Just _`, else `Nothing`. -}
allJust : List Value -> Maybe (List Value)
allJust margs =
    case margs of
        [] ->
            Just []

        (VCtor "Just" [ x ]) :: rest ->
            Maybe.map (\xs -> x :: xs) (allJust rest)

        _ ->
            Nothing


-- ARRAY (a 0-indexed sequence, wrapped as `VCtor "Array" [ VList elems ]`) ------------------------


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


{-| `Array.slice from to`: a half-open range, with negative indices counting back from the end (as in
elm/core). -}
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


{-| Whether a builtin name is one of the `Html.Lazy`/`Svg.Lazy` `lazyN` family — the qualified form
(`Html.Lazy.lazy2`) or the exposed one (`lazy2`), so the last dotted segment is what matters. -}
isLazyBuiltin : String -> Bool
isLazyBuiltin name =
    List.member
        (String.split "." name |> List.reverse |> List.head |> Maybe.withDefault name)
        [ "lazy", "lazy2", "lazy3", "lazy4", "lazy5" ]


{-| The interpreter capabilities handed to each split-out builtin module (see {@link EvalCore}). -}
core : Core
core =
    { apply = applyValue
    , applyAll = applyAllValues
    , mapValues = mapValues
    , filterValues = filterValues
    , foldlValues = foldlValues
    }


{-| The builtin modules split out of `Eval`, keyed by the Elm module they handle (the part of a
builtin name before the first dot). `runBuiltin` dispatches to one of these by {@link moduleOf}, and
`builtinNames`/`arityTable` are aggregated from their `.names`/`.arities`. -}
processors : Dict String Processor
processors =
    Dict.fromList
        [ ( "String", EvalString.processor )
        , ( "Char", EvalChar.processor )
        , ( "Bitwise", EvalBitwise.processor )
        , ( "Debug", EvalDebug.processor )
        , ( "Tuple", EvalTuple.processor )
        ]


{-| Every split-out module's builtin names, folded into {@link builtinNames}. -}
processorNames : List String
processorNames =
    List.concatMap .names (Dict.values processors)


{-| Every split-out module's arity groups, folded into {@link arityTable}. -}
processorArities : List ( Int, List String )
processorArities =
    List.concatMap .arities (Dict.values processors)


{-| The Elm module a qualified builtin belongs to — {@code "String.fromInt" -> "String"}; an
unqualified name (no dot) has no owning module, so {@code ""} (handled by `Eval`'s own cases). -}
moduleOf : String -> String
moduleOf name =
    case String.split "." name of
        first :: _ :: _ ->
            first

        _ ->
            ""


{-| Runs a fully-applied builtin. Html element/attribute builtins produce a structured `Value` tree
(VCtor "Html.node"/"Html.text"/"Html.on"/"Html.style") the editor renders to live Html. Threads
`globals` so higher-order builtins (List.map) can apply the function value they're given. -}
runBuiltin : Globals -> String -> List Value -> Result String Value
runBuiltin globals name args =
    case Dict.get (moduleOf name) processors |> Maybe.andThen (\p -> p.run core globals name args) of
        Just result ->
            result

        Nothing ->
            runBuiltinCore globals name args


runBuiltinCore : Globals -> String -> List Value -> Result String Value
runBuiltinCore globals name args =
    if name == "circle" && EvalPlayground.playgroundCircle args then
        -- Playground `circle color radius` (Svg `circle attrs children` falls through below).
        Ok (EvalPlayground.mkShape (VCtor "PCircle" args))

    else if isLazyBuiltin name then
        -- Html.Lazy.lazyN / Svg.Lazy.lazyN: force it — apply the view function to its arguments. The
        -- interpreter re-renders every frame, so there is nothing to memoise; the result is the node.
        case args of
            f :: rest ->
                applyAllValues globals f rest

            [] ->
                Err (name ++ ": missing view function")

    else if Set.member name playgroundSet then
        EvalPlayground.runPlayground globals name args

    else if name == "onAnimationFrameDelta" then
        -- The editor drives this live: a frame's delta (ms) is fed to its toMsg each animation frame.
        case args of
            [ toMsg ] ->
                Ok (VCtor "Sub.animationFrame" [ toMsg ])

            _ ->
                Ok (VCtor "Sub" [])

    else if name == "onKeyDown" then
        Ok (VCtor "Sub.keyDown" args)

    else if name == "onKeyUp" then
        Ok (VCtor "Sub.keyUp" args)

    else if name == "onResize" then
        Ok (VCtor "Sub.resize" args)

    else if name == "onMouseMove" then
        Ok (VCtor "Sub.mouseMove" args)

    else if Set.member name browserEventSet then
        -- Other Browser.Events subscriptions: opaque no-op subs so the program runs.
        Ok (VCtor "Sub" [])

    else if name == "WebGL.toHtml" then
        case args of
            [ attrs, entities ] ->
                Ok (webglScene attrs entities)

            _ ->
                Err "WebGL.toHtml needs attributes and entities"

    else if name == "WebGL.toHtmlWith" then
        case args of
            [ _, attrs, entities ] ->
                Ok (webglScene attrs entities)

            _ ->
                Err "WebGL.toHtmlWith needs options, attributes and entities"

    else if Set.member name vec3OpsSet then
        -- Linear-algebra on *concrete* vectors is computed for real: the examples' physics needs
        -- numbers (e.g. `Vec3.getY position > eyeLevel`). When an argument isn't a concrete vector
        -- (a symbolic Mat4 result bound for the GPU), fall through to an opaque value instead.
        case vecBuiltin name args of
            Just result ->
                result

            Nothing ->
                Ok (VCtor name args)

    else if Set.member name webglSet then
        -- Meshes, entities, vectors, matrices and textures: opaque values the preview just counts.
        Ok (VCtor name args)

    else if List.member name htmlTags then
        case args of
            [ attrs, children ] ->
                Ok (VCtor "Html.node" [ VStr name, attrs, children ])

            _ ->
                Err (name ++ " needs attributes and children")

    else if List.member name htmlStringAttrs || List.member name htmlBoolAttrs then
        case args of
            [ v ] ->
                Ok (VCtor "Html.attr" [ VStr (EvalRender.attrKey name), v ])

            _ ->
                Err (name ++ " needs a value")

    else
        case ( name, args ) of
            ( "text", [ v ] ) ->
                Ok (VCtor "Html.text" [ v ])

            ( "onClick", [ msg ] ) ->
                Ok (VCtor "Html.on" [ VStr "click", msg ])

            ( "onInput", [ handler ] ) ->
                -- The handler (e.g. a Msg constructor) is applied to the input string at event time.
                Ok (VCtor "Html.on" [ VStr "input", handler ])

            -- Generic event handlers (Html.Events.on / preventDefaultOn / stopPropagationOn). The
            -- editor wires click/input live; other events (e.g. drag/drop) render as inert handlers,
            -- so a program using them — like the image-previews drag target — at least displays.
            ( "on", [ VStr event, handler ] ) ->
                Ok (VCtor "Html.on" [ VStr event, handler ])

            ( "preventDefaultOn", [ VStr event, handler ] ) ->
                Ok (VCtor "Html.on" [ VStr event, handler ])

            ( "stopPropagationOn", [ VStr event, handler ] ) ->
                Ok (VCtor "Html.on" [ VStr event, handler ])

            ( "style", [ k, v ] ) ->
                Ok (VCtor "Html.style" [ k, v ])

            ( "toString", [ VStr s ] ) ->
                Ok (VStr s)

            ( "toString", [ v ] ) ->
                Ok (VStr (renderValue v))

            ( "negate", [ VNum n ] ) ->
                Ok (VNum (negate n))

            ( "not", [ VBool b ] ) ->
                Ok (VBool (not b))

            ( "Browser.sandbox", [ config ] ) ->
                -- The editor drives init/update/view directly; evaluating `main` just yields the config.
                Ok config

            ( "Browser.element", [ config ] ) ->
                Ok config

            ( "List.length", [ VList xs ] ) ->
                Ok (VNum (toFloat (List.length xs)))

            ( "List.sum", [ VList xs ] ) ->
                Ok (VNum (List.sum (List.filterMap asNum xs)))

            ( "List.range", [ VNum a, VNum b ] ) ->
                Ok (VList (List.map (\n -> VNum (toFloat n)) (List.range (round a) (round b))))

            ( "List.map", [ f, VList xs ] ) ->
                mapValues globals f xs |> Result.map VList

            ( "Maybe.withDefault", [ dflt, v ] ) ->
                case v of
                    VCtor "Just" [ x ] ->
                        Ok x

                    VCtor "Nothing" [] ->
                        Ok dflt

                    _ ->
                        Ok dflt

            ( "cos", [ VNum n ] ) ->
                Ok (VNum (cos n))

            ( "sin", [ VNum n ] ) ->
                Ok (VNum (sin n))

            ( "tan", [ VNum n ] ) ->
                Ok (VNum (tan n))

            ( "sqrt", [ VNum n ] ) ->
                Ok (VNum (sqrt n))

            ( "toFloat", [ VNum n ] ) ->
                Ok (VNum n)

            ( "round", [ VNum n ] ) ->
                Ok (VNum (toFloat (round n)))

            ( "floor", [ VNum n ] ) ->
                Ok (VNum (toFloat (floor n)))

            ( "ceiling", [ VNum n ] ) ->
                Ok (VNum (toFloat (ceiling n)))

            ( "truncate", [ VNum n ] ) ->
                Ok (VNum (toFloat (truncate n)))

            ( "abs", [ VNum n ] ) ->
                Ok (VNum (abs n))

            ( "asin", [ VNum n ] ) ->
                Ok (VNum (asin n))

            ( "acos", [ VNum n ] ) ->
                Ok (VNum (acos n))

            ( "atan", [ VNum n ] ) ->
                Ok (VNum (atan n))

            ( "atan2", [ VNum y, VNum x ] ) ->
                Ok (VNum (atan2 y x))

            ( "logBase", [ VNum b, VNum n ] ) ->
                Ok (VNum (logBase b n))

            ( "radians", [ VNum n ] ) ->
                Ok (VNum n)

            ( "turns", [ VNum n ] ) ->
                Ok (VNum (2 * pi * n))

            ( "isNaN", [ VNum n ] ) ->
                Ok (VBool (isNaN n))

            ( "isInfinite", [ VNum n ] ) ->
                Ok (VBool (isInfinite n))

            -- Bitwise ops act on the truncated 32-bit integer value of each number.
            ( "Time.millisToPosix", [ VNum n ] ) ->
                Ok (VNum n)

            ( "Time.posixToMillis", [ VNum n ] ) ->
                Ok (VNum n)

            ( "Time.toHour", [ _, VNum ms ] ) ->
                Ok (VNum (toFloat (modBy 24 (round ms // 3600000))))

            ( "Time.toMinute", [ _, VNum ms ] ) ->
                Ok (VNum (toFloat (modBy 60 (round ms // 60000))))

            ( "Time.toSecond", [ _, VNum ms ] ) ->
                Ok (VNum (toFloat (modBy 60 (round ms // 1000))))

            -- `Time.every interval toMsg` is a subscription the editor inspects to drive a live tick.
            ( "Time.every", [ VNum interval, toMsg ] ) ->
                Ok (VCtor "Sub.every" [ VNum interval, toMsg ])

            -- Random generators carry their spec so the editor can sample them with its own seed.
            ( "Random.int", [ VNum lo, VNum hi ] ) ->
                Ok (VCtor "Random.Gen" [ VStr "int", VNum lo, VNum hi ])

            ( "Random.float", [ VNum lo, VNum hi ] ) ->
                Ok (VCtor "Random.Gen" [ VStr "float", VNum lo, VNum hi ])

            ( "Random.uniform", [ first, VList rest ] ) ->
                Ok (VCtor "Random.Gen" [ VStr "uniform", VList (first :: rest) ])

            ( "Random.constant", [ x ] ) ->
                Ok (VCtor "Random.Gen" [ VStr "constant", x ])

            ( "Random.map", [ f, g ] ) ->
                Ok (VCtor "Random.Gen" [ VStr "map", f, g ])

            ( "Random.map2", [ f, g1, g2 ] ) ->
                Ok (VCtor "Random.Gen" [ VStr "map2", f, g1, g2 ])

            ( "Random.map3", [ f, g1, g2, g3 ] ) ->
                Ok (VCtor "Random.Gen" [ VStr "map3", f, g1, g2, g3 ])

            ( "Random.pair", [ g1, g2 ] ) ->
                Ok (VCtor "Random.Gen" [ VStr "pair", g1, g2 ])

            ( "Random.list", [ VNum n, g ] ) ->
                Ok (VCtor "Random.Gen" [ VStr "list", VNum n, g ])

            ( "Random.andThen", [ f, g ] ) ->
                Ok (VCtor "Random.Gen" [ VStr "andThen", f, g ])

            ( "Random.generate", [ toMsg, gen ] ) ->
                Ok (VCtor "Cmd.random" [ toMsg, gen ])

            ( "Http.expectString", [ toMsg ] ) ->
                Ok (VCtor "Http.expect" [ toMsg ])

            ( "Http.expectJson", [ toMsg, decoder ] ) ->
                Ok (VCtor "Http.expectJson" [ toMsg, decoder ])

            ( "Http.get", [ VRecord fields ] ) ->
                -- A GET command the editor issues for real, feeding the response back via `expect`.
                case ( lookup "url" fields, lookup "expect" fields ) of
                    ( Just (VStr url), Just expect ) ->
                        Ok (VCtor "Cmd.http" [ VStr url, expect ])

                    _ ->
                        Err "Http.get needs { url : String, expect : … }"

            -- File -----------------------------------------------------------------------------
            -- `File.Select.file mimes toMsg` is a command the editor runs by opening a real browser
            -- file picker; the chosen file (a `VCtor "File" [name, content]`) is fed back via toMsg.
            ( "File.Select.file", [ _, toMsg ] ) ->
                Ok (VCtor "Cmd.fileSelect" [ toMsg ])

            ( "File.Select.files", [ _, toMsg ] ) ->
                -- `files` takes `File -> List File -> msg`; flag it so the picked file is delivered as
                -- (file, []) rather than only the first argument (which left an unsaturated message).
                Ok (VCtor "Cmd.fileSelectMany" [ toMsg ])

            ( "File.name", [ VCtor "File" [ name, _ ] ] ) ->
                Ok name

            ( "File.mime", [ VCtor "File" _ ] ) ->
                Ok (VStr "text/plain")

            ( "File.size", [ VCtor "File" [ _, VStr content ] ] ) ->
                Ok (VNum (toFloat (String.length content)))

            -- Reading a file's contents: a Task the editor resolves immediately (it already has the
            -- text), so `Task.perform GotContent (File.toString file)` delivers the content.
            ( "File.toString", [ VCtor "File" [ _, content ] ] ) ->
                Ok (VCtor "Task.value" [ content ])

            ( "File.toUrl", [ VCtor "File" [ _, content ] ] ) ->
                Ok (VCtor "Task.value" [ content ])

            ( "Task.perform", [ toMsg, task ] ) ->
                Ok (VCtor "Cmd.task" [ toMsg, task ])

            ( "Task.attempt", [ toMsg, task ] ) ->
                Ok (VCtor "Cmd.taskAttempt" [ toMsg, task ])

            ( "field", [ VStr name2, decoder ] ) ->
                Ok (VCtor "Dec.field" [ VStr name2, decoder ])

            ( "at", [ VList path, decoder ] ) ->
                -- `at [ "a", "b" ] dec` is sugar for nested fields: field "a" (field "b" dec).
                Ok
                    (List.foldr
                        (\seg acc ->
                            case seg of
                                VStr name2 ->
                                    VCtor "Dec.field" [ VStr name2, acc ]

                                _ ->
                                    acc
                        )
                        decoder
                        path
                    )

            ( "map", [ f, dec ] ) ->
                Ok (VCtor "Dec.map" [ f, dec ])

            ( "oneOrMore", [ f, dec ] ) ->
                Ok (VCtor "Dec.oneOrMore" [ f, dec ])

            ( "succeed", [ v ] ) ->
                Ok (VCtor "Dec.succeed" [ v ])

            ( "map2", [ f, a, b ] ) ->
                Ok (VCtor "Dec.map" [ f, a, b ])

            ( "map3", [ f, a, b, c ] ) ->
                Ok (VCtor "Dec.map" [ f, a, b, c ])

            ( "map4", [ f, a, b, c, d ] ) ->
                Ok (VCtor "Dec.map" [ f, a, b, c, d ])

            ( "map5", [ f, a, b, c, d, e ] ) ->
                Ok (VCtor "Dec.map" [ f, a, b, c, d, e ])

            ( "map6", [ f, a, b, c, d, e, g ] ) ->
                Ok (VCtor "Dec.map" [ f, a, b, c, d, e, g ])

            ( "map7", [ f, a, b, c, d, e, g, h ] ) ->
                Ok (VCtor "Dec.map" [ f, a, b, c, d, e, g, h ])

            ( "map8", [ f, a, b, c, d, e, g, h, i ] ) ->
                Ok (VCtor "Dec.map" [ f, a, b, c, d, e, g, h, i ])

            ( "list", [ dec ] ) ->
                Ok (VCtor "Dec.list" [ dec ])

            ( "andThen", [ f, dec ] ) ->
                Ok (VCtor "Dec.andThen" [ f, dec ])

            ( "oneOf", [ decs ] ) ->
                Ok (VCtor "Dec.oneOf" [ decs ])

            ( "nullable", [ dec ] ) ->
                Ok (VCtor "Dec.nullable" [ dec ])

            ( "Encode.int", [ v ] ) ->
                Ok v

            ( "Encode.float", [ v ] ) ->
                Ok v

            ( "Encode.string", [ v ] ) ->
                Ok v

            ( "Encode.bool", [ v ] ) ->
                Ok v

            ( "Encode.object", [ pairs ] ) ->
                Ok (EvalJson.encodeObject pairs)

            ( "Encode.list", [ f, xs ] ) ->
                EvalJson.encodeList applyValue globals f xs

            ( "Encode.encode", [ _, value ] ) ->
                Ok (VStr (EvalJson.jsonEncode value))

            -- List ---------------------------------------------------------------------------
            ( "List.reverse", [ VList xs ] ) ->
                Ok (VList (List.reverse xs))

            ( "List.head", [ VList xs ] ) ->
                Ok (maybeValue (List.head xs))

            ( "List.tail", [ VList xs ] ) ->
                Ok (maybeValue (Maybe.map VList (List.tail xs)))

            ( "List.isEmpty", [ VList xs ] ) ->
                Ok (VBool (List.isEmpty xs))

            ( "List.maximum", [ VList xs ] ) ->
                Ok (maybeValue (Maybe.map VNum (List.maximum (List.filterMap asNum xs))))

            ( "List.minimum", [ VList xs ] ) ->
                Ok (maybeValue (Maybe.map VNum (List.minimum (List.filterMap asNum xs))))

            ( "List.sort", [ VList xs ] ) ->
                Ok (VList (List.sortWith valueCompare xs))

            ( "List.sortBy", [ f, VList xs ] ) ->
                -- Keys are computed once per element, then compared (a Schwartzian transform).
                keyValues globals f xs
                    |> Result.map (\keyed -> VList (List.map Tuple.second (List.sortWith (\a b -> valueCompare (Tuple.first a) (Tuple.first b)) keyed)))

            ( "List.concat", [ VList xs ] ) ->
                Ok (VList (List.concatMap asList xs))

            ( "List.product", [ VList xs ] ) ->
                Ok (VNum (List.product (List.filterMap asNum xs)))

            ( "List.append", [ VList a, VList b ] ) ->
                Ok (VList (a ++ b))

            ( "List.member", [ x, VList xs ] ) ->
                Ok (VBool (List.any (valueEq x) xs))

            ( "List.filter", [ f, VList xs ] ) ->
                filterValues globals f xs |> Result.map VList

            ( "List.filterMap", [ f, VList xs ] ) ->
                mapValues globals f xs |> Result.map (\ys -> VList (List.filterMap keepJust ys))

            ( "List.concatMap", [ f, VList xs ] ) ->
                mapValues globals f xs |> Result.map (\ys -> VList (List.concatMap asList ys))

            ( "List.take", [ VNum n, VList xs ] ) ->
                Ok (VList (List.take (round n) xs))

            ( "List.drop", [ VNum n, VList xs ] ) ->
                Ok (VList (List.drop (round n) xs))

            ( "List.repeat", [ VNum n, x ] ) ->
                Ok (VList (List.repeat (round n) x))

            ( "List.singleton", [ x ] ) ->
                Ok (VList [ x ])

            ( "List.any", [ f, VList xs ] ) ->
                anyValues globals f xs |> Result.map VBool

            ( "List.all", [ f, VList xs ] ) ->
                allValues globals f xs |> Result.map VBool

            ( "List.indexedMap", [ f, VList xs ] ) ->
                indexedMapValues globals f 0 xs |> Result.map VList

            ( "List.map2", [ f, VList a, VList b ] ) ->
                map2Values globals f a b |> Result.map VList

            ( "List.foldl", [ f, acc, VList xs ] ) ->
                foldlValues globals f acc xs

            ( "List.foldr", [ f, acc, VList xs ] ) ->
                foldlValues globals f acc (List.reverse xs)

            -- Maybe / Result -----------------------------------------------------------------
            ( "Maybe.map", [ f, v ] ) ->
                case v of
                    VCtor "Just" [ x ] ->
                        applyValue globals f x |> Result.map (\y -> VCtor "Just" [ y ])

                    _ ->
                        Ok (VCtor "Nothing" [])

            ( "Maybe.andThen", [ f, v ] ) ->
                case v of
                    VCtor "Just" [ x ] ->
                        applyValue globals f x

                    _ ->
                        Ok (VCtor "Nothing" [])

            ( "Maybe.map2", [ f, va, vb ] ) ->
                case ( va, vb ) of
                    ( VCtor "Just" [ a ], VCtor "Just" [ b ] ) ->
                        applyValue globals f a |> Result.andThen (\g -> applyValue globals g b) |> Result.map (\y -> VCtor "Just" [ y ])

                    _ ->
                        Ok (VCtor "Nothing" [])

            ( "Maybe.map3", [ f, a, b, c ] ) ->
                maybeMapN globals f [ a, b, c ]

            ( "Maybe.map4", [ f, a, b, c, d ] ) ->
                maybeMapN globals f [ a, b, c, d ]

            ( "Maybe.map5", [ f, a, b, c, d, e ] ) ->
                maybeMapN globals f [ a, b, c, d, e ]

            ( "Result.withDefault", [ dflt, v ] ) ->
                case v of
                    VCtor "Ok" [ x ] ->
                        Ok x

                    _ ->
                        Ok dflt

            ( "Result.map", [ f, v ] ) ->
                case v of
                    VCtor "Ok" [ x ] ->
                        applyValue globals f x |> Result.map (\y -> VCtor "Ok" [ y ])

                    _ ->
                        Ok v

            ( "Result.andThen", [ f, v ] ) ->
                case v of
                    VCtor "Ok" [ x ] ->
                        applyValue globals f x

                    _ ->
                        Ok v

            ( "Result.toMaybe", [ v ] ) ->
                case v of
                    VCtor "Ok" [ x ] ->
                        Ok (VCtor "Just" [ x ])

                    _ ->
                        Ok (VCtor "Nothing" [])

            ( "Result.mapError", [ f, v ] ) ->
                case v of
                    VCtor "Err" [ x ] ->
                        applyValue globals f x |> Result.map (\y -> VCtor "Err" [ y ])

                    _ ->
                        Ok v

            ( "Result.fromMaybe", [ err, v ] ) ->
                case v of
                    VCtor "Just" [ x ] ->
                        Ok (VCtor "Ok" [ x ])

                    _ ->
                        Ok (VCtor "Err" [ err ])

            ( "Result.map2", [ f, va, vb ] ) ->
                case ( va, vb ) of
                    ( VCtor "Ok" [ a ], VCtor "Ok" [ b ] ) ->
                        applyValue globals f a |> Result.andThen (\g -> applyValue globals g b) |> Result.map (\y -> VCtor "Ok" [ y ])

                    ( VCtor "Err" [ x ], _ ) ->
                        Ok (VCtor "Err" [ x ])

                    ( _, err ) ->
                        Ok err

            ( "Result.map4", [ f, a, b, c, d ] ) ->
                resultMapN globals f [ a, b, c, d ]

            ( "Result.map5", [ f, a, b, c, d, e ] ) ->
                resultMapN globals f [ a, b, c, d, e ]

            ( "Result.map3", [ f, va, vb, vc ] ) ->
                case ( va, vb, vc ) of
                    ( VCtor "Ok" [ a ], VCtor "Ok" [ b ], VCtor "Ok" [ c ] ) ->
                        applyValue globals f a
                            |> Result.andThen (\g -> applyValue globals g b)
                            |> Result.andThen (\h -> applyValue globals h c)
                            |> Result.map (\y -> VCtor "Ok" [ y ])

                    ( VCtor "Err" [ x ], _, _ ) ->
                        Ok (VCtor "Err" [ x ])

                    ( _, VCtor "Err" [ x ], _ ) ->
                        Ok (VCtor "Err" [ x ])

                    ( _, _, err ) ->
                        Ok err

            -- Tuple / Basics -----------------------------------------------------------------
            ( "xor", [ VBool a, VBool b ] ) ->
                Ok (VBool (xor a b))

            ( "identity", [ v ] ) ->
                Ok v

            ( "always", [ v, _ ] ) ->
                Ok v

            ( "min", [ VNum a, VNum b ] ) ->
                Ok (VNum (Basics.min a b))

            ( "max", [ VNum a, VNum b ] ) ->
                Ok (VNum (Basics.max a b))

            ( "clamp", [ VNum lo, VNum hi, VNum x ] ) ->
                Ok (VNum (Basics.clamp lo hi x))

            ( "modBy", [ VNum m, VNum n ] ) ->
                if round m == 0 then
                    Err "modBy: division by zero"

                else
                    Ok (VNum (toFloat (modBy (round m) (round n))))

            ( "remainderBy", [ VNum m, VNum n ] ) ->
                if round m == 0 then
                    Err "remainderBy: division by zero"

                else
                    Ok (VNum (toFloat (remainderBy (round m) (round n))))

            -- String -------------------------------------------------------------------------
            ( "List.intersperse", [ sep, VList xs ] ) ->
                Ok (VList (List.intersperse sep xs))

            ( "List.partition", [ f, VList xs ] ) ->
                partitionValues globals f xs

            ( "List.unzip", [ VList xs ] ) ->
                Ok
                    (VTup
                        [ VList (List.filterMap pairKey xs)
                        , VList (List.filterMap pairValue xs)
                        ]
                    )

            ( "List.map3", [ f, VList xs, VList ys, VList zs ] ) ->
                map3Values globals f xs ys zs |> Result.map VList

            ( "List.map4", [ f, VList a, VList b, VList c, VList d ] ) ->
                mapNValues globals f [ a, b, c, d ] |> Result.map VList

            ( "List.map5", [ f, VList a, VList b, VList c, VList d, VList e ] ) ->
                mapNValues globals f [ a, b, c, d, e ] |> Result.map VList

            ( "List.sortWith", [ f, VList xs ] ) ->
                Ok (VList (List.sortWith (orderFromCompare globals f) xs))

            ( "compare", [ a, b ] ) ->
                Ok (orderValue (valueCompare a b))

            -- Dict: a Dict is `VCtor "Dict" [ VList pairs ]` where each pair is `VTup [ key, value ]`
            -- and keys are unique (an association list; lookups scan it).
            ( "Dict.empty", [] ) ->
                Ok (mkDict [])

            ( "Dict.singleton", [ k, v ] ) ->
                Ok (mkDict [ VTup [ k, v ] ])

            ( "Dict.fromList", [ VList ps ] ) ->
                Ok (List.foldl (\p acc -> dictInsertPair p acc) (mkDict []) ps)

            ( "Dict.toList", [ d ] ) ->
                Ok (VList (dictPairs d))

            ( "Dict.keys", [ d ] ) ->
                Ok (VList (List.filterMap pairKey (dictPairs d)))

            ( "Dict.values", [ d ] ) ->
                Ok (VList (List.filterMap pairValue (dictPairs d)))

            ( "Dict.size", [ d ] ) ->
                Ok (VNum (toFloat (List.length (dictPairs d))))

            ( "Dict.isEmpty", [ d ] ) ->
                Ok (VBool (List.isEmpty (dictPairs d)))

            ( "Dict.member", [ k, d ] ) ->
                Ok (VBool (dictGet k (dictPairs d) /= Nothing))

            ( "Dict.get", [ k, d ] ) ->
                Ok (maybeValue (dictGet k (dictPairs d)))

            ( "Dict.insert", [ k, v, d ] ) ->
                Ok (mkDict (dictSet k v (dictPairs d)))

            ( "Dict.remove", [ k, d ] ) ->
                Ok (mkDict (List.filter (\p -> not (pairKeyEq k p)) (dictPairs d)))

            ( "Dict.map", [ f, d ] ) ->
                mapDict globals f (dictPairs d)

            ( "Dict.filter", [ f, d ] ) ->
                filterDict globals f (dictPairs d)

            ( "Dict.foldl", [ f, acc, d ] ) ->
                foldlDict globals f acc (dictPairs d)

            ( "Dict.foldr", [ f, acc, d ] ) ->
                foldlDict globals f acc (List.reverse (dictPairs d))

            ( "Dict.partition", [ f, d ] ) ->
                -- (matching, non-matching) by the key/value predicate, preserving order
                partitionDict globals f (dictPairs d)

            ( "Dict.union", [ a, b ] ) ->
                -- Left-biased: a's entries win on a key collision.
                let
                    aKeys =
                        List.filterMap pairKey (dictPairs a)
                in
                Ok (mkDict (dictPairs a ++ List.filter (\p -> not (List.any (\k -> pairKeyEq k p) aKeys)) (dictPairs b)))

            ( "Dict.diff", [ a, b ] ) ->
                let
                    bKeys =
                        List.filterMap pairKey (dictPairs b)
                in
                Ok (mkDict (List.filter (\p -> not (List.any (\k -> pairKeyEq k p) bKeys)) (dictPairs a)))

            ( "Dict.intersect", [ a, b ] ) ->
                let
                    bKeys =
                        List.filterMap pairKey (dictPairs b)
                in
                Ok (mkDict (List.filter (\p -> List.any (\k -> pairKeyEq k p) bKeys) (dictPairs a)))

            ( "Dict.update", [ k, f, d ] ) ->
                -- f : Maybe v -> Maybe v; Just replaces/inserts, Nothing removes the key.
                applyValue globals f (maybeValue (dictGet k (dictPairs d)))
                    |> Result.map
                        (\res ->
                            case res of
                                VCtor "Just" [ v ] ->
                                    mkDict (dictSet k v (dictPairs d))

                                _ ->
                                    mkDict (List.filter (\p -> not (pairKeyEq k p)) (dictPairs d))
                        )

            -- Set: a Set is `VCtor "Set" [ VList elems ]` with unique elements (insertion order).
            ( "Set.empty", [] ) ->
                Ok (mkSet [])

            ( "Set.singleton", [ x ] ) ->
                Ok (mkSet [ x ])

            ( "Set.fromList", [ VList xs ] ) ->
                Ok (mkSet (List.foldl setInsert [] xs))

            ( "Set.toList", [ s ] ) ->
                Ok (VList (setElems s))

            ( "Set.size", [ s ] ) ->
                Ok (VNum (toFloat (List.length (setElems s))))

            ( "Set.isEmpty", [ s ] ) ->
                Ok (VBool (List.isEmpty (setElems s)))

            ( "Set.member", [ x, s ] ) ->
                Ok (VBool (List.any (valueEq x) (setElems s)))

            ( "Set.insert", [ x, s ] ) ->
                Ok (mkSet (setInsert x (setElems s)))

            ( "Set.remove", [ x, s ] ) ->
                Ok (mkSet (List.filter (\y -> not (valueEq x y)) (setElems s)))

            ( "Set.union", [ a, b ] ) ->
                Ok (mkSet (List.foldl setInsert (setElems a) (setElems b)))

            ( "Set.intersect", [ a, b ] ) ->
                Ok (mkSet (List.filter (\x -> List.any (valueEq x) (setElems b)) (setElems a)))

            ( "Set.diff", [ a, b ] ) ->
                Ok (mkSet (List.filter (\x -> not (List.any (valueEq x) (setElems b))) (setElems a)))

            ( "Set.foldl", [ f, acc, s ] ) ->
                foldlValues globals f acc (setElems s)

            ( "Set.foldr", [ f, acc, s ] ) ->
                foldlValues globals f acc (List.reverse (setElems s))

            ( "Set.map", [ f, s ] ) ->
                mapValues globals f (setElems s) |> Result.map (\ys -> mkSet (List.foldl setInsert [] ys))

            ( "Set.filter", [ f, s ] ) ->
                filterValues globals f (setElems s) |> Result.map mkSet

            ( "Set.partition", [ f, s ] ) ->
                filterValues globals f (setElems s)
                    |> Result.map
                        (\yes ->
                            VTup
                                [ mkSet yes
                                , mkSet (List.filter (\x -> not (List.any (valueEq x) yes)) (setElems s))
                                ]
                        )

            -- Debug: toString renders any value; log returns its value (no console in the editor);
            -- todo aborts with the message (as Elm's Debug.todo crashes at runtime).
            ( "Array.empty", [] ) ->
                Ok (mkArray [])

            ( "Array.fromList", [ VList xs ] ) ->
                Ok (mkArray xs)

            ( "Array.toList", [ a ] ) ->
                Ok (VList (arrayElems a))

            ( "Array.toIndexedList", [ a ] ) ->
                Ok (VList (List.indexedMap (\i x -> VTup [ VNum (toFloat i), x ]) (arrayElems a)))

            ( "Array.length", [ a ] ) ->
                Ok (VNum (toFloat (List.length (arrayElems a))))

            ( "Array.isEmpty", [ a ] ) ->
                Ok (VBool (List.isEmpty (arrayElems a)))

            ( "Array.repeat", [ VNum n, x ] ) ->
                Ok (mkArray (List.repeat (round n) x))

            ( "Array.initialize", [ VNum n, f ] ) ->
                mapValues globals f (List.map (\i -> VNum (toFloat i)) (List.range 0 (round n - 1)))
                    |> Result.map mkArray

            ( "Array.get", [ VNum i, a ] ) ->
                let
                    xs =
                        arrayElems a

                    idx =
                        round i
                in
                if idx >= 0 && idx < List.length xs then
                    Ok (maybeValue (List.head (List.drop idx xs)))

                else
                    Ok (VCtor "Nothing" [])

            ( "Array.set", [ VNum i, x, a ] ) ->
                let
                    idx =
                        round i
                in
                Ok (mkArray (List.indexedMap (\j y -> if j == idx then x else y) (arrayElems a)))

            ( "Array.push", [ x, a ] ) ->
                Ok (mkArray (arrayElems a ++ [ x ]))

            ( "Array.append", [ a, b ] ) ->
                Ok (mkArray (arrayElems a ++ arrayElems b))

            ( "Array.slice", [ VNum from, VNum to, a ] ) ->
                Ok (mkArray (arraySlice (round from) (round to) (arrayElems a)))

            ( "Array.map", [ f, a ] ) ->
                mapValues globals f (arrayElems a) |> Result.map mkArray

            ( "Array.indexedMap", [ f, a ] ) ->
                indexedMapValues globals f 0 (arrayElems a) |> Result.map mkArray

            ( "Array.filter", [ f, a ] ) ->
                filterValues globals f (arrayElems a) |> Result.map mkArray

            ( "Array.foldl", [ f, acc, a ] ) ->
                foldlValues globals f acc (arrayElems a)

            ( "Array.foldr", [ f, acc, a ] ) ->
                foldlValues globals f acc (List.reverse (arrayElems a))

            _ ->
                Err ("bad arguments to " ++ name)


{-| The editor's preview for a `WebGL.toHtml` scene: a labelled box reporting the entity count
(the small interpreter can't run GPU shaders; the JS backend renders WebGL for real). -}
{-| A WebGL scene as a structured value: the canvas attributes and the list of entities (each an
opaque `WebGL.entity` value carrying its shaders/mesh/uniforms). The editor renders this live by
handing the entities to the JS WebGL runtime; the Java interpreter keeps it as data for tests. -}
webglScene : Value -> Value -> Value
webglScene attrs entities =
    VCtor "WebGL.scene" [ attrs, entities ]


{-| The linear-algebra builtins that are evaluated on concrete vectors (rather than kept opaque for
the GPU). `vecBuiltin` returns `Nothing` when an argument isn't a concrete `vec2`/`vec3`. -}
vec3Ops : List String
vec3Ops =
    [ "Vec3.getX", "Vec3.getY", "Vec3.getZ", "Vec3.setX", "Vec3.setY", "Vec3.setZ" ]
        ++ [ "Vec3.add", "Vec3.sub", "Vec3.scale", "Vec3.negate", "Vec3.dot", "Vec3.length" ]
        ++ [ "Vec3.distance", "Vec3.normalize", "Vec3.cross", "Vec3.direction" ]
        ++ [ "Vec2.getX", "Vec2.getY" ]


vec3Of : Value -> Maybe ( Float, Float, Float )
vec3Of v =
    case v of
        VCtor "vec3" [ VNum x, VNum y, VNum z ] ->
            Just ( x, y, z )

        _ ->
            Nothing


vec2Of : Value -> Maybe ( Float, Float )
vec2Of v =
    case v of
        VCtor "vec2" [ VNum x, VNum y ] ->
            Just ( x, y )

        _ ->
            Nothing


mkVec3 : Float -> Float -> Float -> Value
mkVec3 x y z =
    VCtor "vec3" [ VNum x, VNum y, VNum z ]


vec3Map2 : (( Float, Float, Float ) -> ( Float, Float, Float ) -> Value) -> Value -> Value -> Maybe (Result String Value)
vec3Map2 f a b =
    Maybe.map2 (\va vb -> Ok (f va vb)) (vec3Of a) (vec3Of b)


vecBuiltin : String -> List Value -> Maybe (Result String Value)
vecBuiltin name args =
    case ( name, args ) of
        ( "Vec3.getX", [ v ] ) ->
            Maybe.map (\( x, _, _ ) -> Ok (VNum x)) (vec3Of v)

        ( "Vec3.getY", [ v ] ) ->
            Maybe.map (\( _, y, _ ) -> Ok (VNum y)) (vec3Of v)

        ( "Vec3.getZ", [ v ] ) ->
            Maybe.map (\( _, _, z ) -> Ok (VNum z)) (vec3Of v)

        ( "Vec3.setX", [ VNum n, v ] ) ->
            Maybe.map (\( _, y, z ) -> Ok (mkVec3 n y z)) (vec3Of v)

        ( "Vec3.setY", [ VNum n, v ] ) ->
            Maybe.map (\( x, _, z ) -> Ok (mkVec3 x n z)) (vec3Of v)

        ( "Vec3.setZ", [ VNum n, v ] ) ->
            Maybe.map (\( x, y, _ ) -> Ok (mkVec3 x y n)) (vec3Of v)

        ( "Vec3.scale", [ VNum s, v ] ) ->
            Maybe.map (\( x, y, z ) -> Ok (mkVec3 (s * x) (s * y) (s * z))) (vec3Of v)

        ( "Vec3.negate", [ v ] ) ->
            Maybe.map (\( x, y, z ) -> Ok (mkVec3 (negate x) (negate y) (negate z))) (vec3Of v)

        ( "Vec3.add", [ a, b ] ) ->
            vec3Map2 (\( ax, ay, az ) ( bx, by, bz ) -> mkVec3 (ax + bx) (ay + by) (az + bz)) a b

        ( "Vec3.sub", [ a, b ] ) ->
            vec3Map2 (\( ax, ay, az ) ( bx, by, bz ) -> mkVec3 (ax - bx) (ay - by) (az - bz)) a b

        ( "Vec3.cross", [ a, b ] ) ->
            vec3Map2 (\( ax, ay, az ) ( bx, by, bz ) -> mkVec3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)) a b

        ( "Vec3.dot", [ a, b ] ) ->
            Maybe.map2 (\( ax, ay, az ) ( bx, by, bz ) -> Ok (VNum (ax * bx + ay * by + az * bz))) (vec3Of a) (vec3Of b)

        ( "Vec3.length", [ v ] ) ->
            Maybe.map (\( x, y, z ) -> Ok (VNum (sqrt (x * x + y * y + z * z)))) (vec3Of v)

        ( "Vec3.distance", [ a, b ] ) ->
            Maybe.map2 (\( ax, ay, az ) ( bx, by, bz ) -> Ok (VNum (sqrt ((ax - bx) ^ 2 + (ay - by) ^ 2 + (az - bz) ^ 2)))) (vec3Of a) (vec3Of b)

        ( "Vec3.normalize", [ v ] ) ->
            Maybe.map (\( x, y, z ) -> Ok (normalizeVec3 x y z)) (vec3Of v)

        ( "Vec3.direction", [ a, b ] ) ->
            Maybe.map2 (\( ax, ay, az ) ( bx, by, bz ) -> Ok (normalizeVec3 (ax - bx) (ay - by) (az - bz))) (vec3Of a) (vec3Of b)

        ( "Vec2.getX", [ v ] ) ->
            Maybe.map (\( x, _ ) -> Ok (VNum x)) (vec2Of v)

        ( "Vec2.getY", [ v ] ) ->
            Maybe.map (\( _, y ) -> Ok (VNum y)) (vec2Of v)

        _ ->
            Nothing


normalizeVec3 : Float -> Float -> Float -> Value
normalizeVec3 x y z =
    let
        len =
            sqrt (x * x + y * y + z * z)
    in
    if len == 0 then
        mkVec3 0 0 0

    else
        mkVec3 (x / len) (y / len) (z / len)


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


{-| Splits a list into (matching, non-matching) by `f`. -}
partitionValues : Globals -> Value -> List Value -> Result String Value
partitionValues globals f xs =
    case xs of
        [] ->
            Ok (VTup [ VList [], VList [] ])

        x :: rest ->
            applyValue globals f x
                |> Result.andThen
                    (\keep ->
                        partitionValues globals f rest
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


{-| `List.map3`: applies `f` across three lists, stopping at the shortest. -}
map3Values : Globals -> Value -> List Value -> List Value -> List Value -> Result String (List Value)
map3Values globals f xs ys zs =
    case ( xs, ys, zs ) of
        ( x :: xr, y :: yr, z :: zr ) ->
            applyValue globals f x
                |> Result.andThen (\g -> applyValue globals g y)
                |> Result.andThen (\h -> applyValue globals h z)
                |> Result.andThen
                    (\v -> map3Values globals f xr yr zr |> Result.map (\vs -> v :: vs))

        _ ->
            Ok []


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


{-| True if the predicate holds for any element. -}
anyValues : Globals -> Value -> List Value -> Result String Bool
anyValues globals f xs =
    case xs of
        [] ->
            Ok False

        x :: rest ->
            applyValue globals f x
                |> Result.andThen
                    (\b ->
                        if b == VBool True then
                            Ok True

                        else
                            anyValues globals f rest
                    )


{-| True if the predicate holds for every element. -}
allValues : Globals -> Value -> List Value -> Result String Bool
allValues globals f xs =
    case xs of
        [] ->
            Ok True

        x :: rest ->
            applyValue globals f x
                |> Result.andThen
                    (\b ->
                        if b == VBool True then
                            allValues globals f rest

                        else
                            Ok False
                    )


{-| `List.indexedMap`: applies `f index element`, threading the index. -}
indexedMapValues : Globals -> Value -> Int -> List Value -> Result String (List Value)
indexedMapValues globals f i xs =
    case xs of
        [] ->
            Ok []

        x :: rest ->
            applyValue globals f (VNum (toFloat i))
                |> Result.andThen (\g -> applyValue globals g x)
                |> Result.andThen (\y -> indexedMapValues globals f (i + 1) rest |> Result.map (\ys -> y :: ys))


{-| `List.map2`: applies `f a b` pairwise, stopping at the shorter list. -}
map2Values : Globals -> Value -> List Value -> List Value -> Result String (List Value)
map2Values globals f xs ys =
    case ( xs, ys ) of
        ( x :: xrest, y :: yrest ) ->
            applyValue globals f x
                |> Result.andThen (\g -> applyValue globals g y)
                |> Result.andThen (\z -> map2Values globals f xrest yrest |> Result.map (\zs -> z :: zs))

        _ ->
            Ok []


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


{-| Pairs each element with the key `f element` produces (for `List.sortBy`). -}
keyValues : Globals -> Value -> List Value -> Result String (List ( Value, Value ))
keyValues globals f xs =
    case xs of
        [] ->
            Ok []

        x :: rest ->
            applyValue globals f x
                |> Result.andThen (\k -> keyValues globals f rest |> Result.map (\ks -> ( k, x ) :: ks))


{-| A total ordering on values, used by `List.sort`/`sortBy` (numbers, strings, then bools). -}
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
    EvalRender.renderValue


{-| Re-exposed from EvalRender so `Eval.htmlToString` stays available (the JS-backend test driver
calls it on a rendered view). -}
htmlToString : Value -> String
htmlToString =
    EvalRender.htmlToString



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
                EvalJson.parseJson jsonText |> Result.andThen (\json -> EvalJson.runDecoder applyValue globals decoder json)
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
                    case EvalJson.parseJson text |> Result.andThen (\json -> EvalJson.runDecoder applyValue globals decoder json) of
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
-- EvalPlayground and re-expose the game functions on Eval's public surface. The pure Playground
-- helpers (EvalPlayground.runPlayground/mkShape/playgroundColor/…) are called qualified at their
-- use sites; only these game functions need a local definition (for the injection + re-export).


gameInitMem : List ( String, String ) -> Maybe Value
gameInitMem files =
    EvalPlayground.gameInitMem mainValue files


gameView : List ( String, String ) -> List String -> Float -> Value -> Result String Value
gameView files keys time mem =
    EvalPlayground.gameView mainValue applyValue files keys time mem


gameStep : List ( String, String ) -> List String -> Float -> Value -> Result String Value
gameStep files keys time mem =
    EvalPlayground.gameStep mainValue applyValue files keys time mem
