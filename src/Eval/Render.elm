module Eval.Render exposing (processor, attrKey, htmlToString, renderStr, renderValue)

{-| Display helpers for the editor's interpreter: turning an interpreted `Value` (and the Html-value
trees the evaluator builds) into the text the REPL path and the result pane show. These are pure
`Value -> String` renderers with no dependency on evaluation, so they live apart from `Eval`'s
mutually-recursive core; `Eval` re-exposes `renderValue` and calls `htmlToString`/`attrKey`.

It also owns the Html element/attribute *builtins* as a {@link Eval.Core.Processor} — `div`/`span`/…
build `Html.node` value trees, the attribute names build `Html.attr`, and `text`/`onClick`/… build
text and event nodes — which `htmlToString` then renders. -}

import Eval.Core exposing (Core, Processor)
import Lang exposing (Globals, Value(..))


{-| The Html (and Svg) element/attribute/event builtins, as a {@link Eval.Core.Processor}. They build
the `Html.node`/`Html.attr`/`Html.text`/`Html.on`/`Html.style` value trees `htmlToString` renders. -}
processor : Processor
processor =
    { names = htmlTags ++ htmlStringAttrs ++ htmlBoolAttrs ++ [ "text", "onClick", "onInput", "on", "preventDefaultOn", "stopPropagationOn", "style" ]
    , arities = [ ( 1, htmlStringAttrs ++ htmlBoolAttrs ++ [ "text", "onClick", "onInput" ] ) ]
    , run = run
    }


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    if List.member name htmlTags then
        case args of
            [ attrs, children ] ->
                Just (Ok (VCtor "Html.node" [ VStr name, attrs, children ]))

            _ ->
                Just (Err (name ++ " needs attributes and children"))

    else if List.member name htmlStringAttrs || List.member name htmlBoolAttrs then
        case args of
            [ v ] ->
                Just (Ok (VCtor "Html.attr" [ VStr (attrKey name), v ]))

            _ ->
                Just (Err (name ++ " needs a value"))

    else
        case ( name, args ) of
            ( "text", [ v ] ) ->
                Just (Ok (VCtor "Html.text" [ v ]))

            ( "onClick", [ msg ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "click", msg ]))

            ( "onInput", [ handler ] ) ->
                -- The handler (e.g. a Msg constructor) is applied to the input string at event time.
                Just (Ok (VCtor "Html.on" [ VStr "input", handler ]))

            -- Generic event handlers (Html.Events.on / preventDefaultOn / stopPropagationOn). The
            -- editor wires click/input live; other events render as inert handlers so programs display.
            ( "on", [ VStr event, handler ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr event, handler ]))

            ( "preventDefaultOn", [ VStr event, handler ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr event, handler ]))

            ( "stopPropagationOn", [ VStr event, handler ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr event, handler ]))

            ( "style", [ k, v ] ) ->
                Just (Ok (VCtor "Html.style" [ k, v ]))

            _ ->
                Nothing


{-| The Html/Svg element tags that build a `Html.node` (`circle` here is the SVG circle; the
playground `circle` is disambiguated by Eval.Playground). -}
htmlTags : List String
htmlTags =
    [ "div", "button", "p", "span", "h1", "h2", "h3", "h4", "h5", "h6", "ul", "ol", "li", "pre", "code", "input", "textarea", "select", "option", "label", "a", "section", "strong", "em", "br", "img", "table", "tr", "td", "th", "blockquote", "cite", "hr", "nav", "header", "footer" ]
        ++ [ "svg", "circle", "rect", "line", "ellipse", "polygon", "polyline", "path", "g", "text_", "defs", "stop", "linearGradient", "radialGradient" ]


{-| `Html.Attributes` / `Svg.Attributes` taking a single string, rendered as `key=value`. -}
htmlStringAttrs : List String
htmlStringAttrs =
    [ "placeholder", "value", "type_", "class", "id", "href", "src", "title", "alt", "name", "for", "target", "rel", "width", "height", "rows", "cols", "autocomplete", "step" ]
        ++ [ "viewBox", "cx", "cy", "r", "x", "y", "x1", "y1", "x2", "y2", "rx", "ry", "fill", "stroke", "points", "d", "transform", "offset", "opacity" ]
        ++ [ "strokeWidth", "strokeLinecap", "strokeDasharray", "fillOpacity", "stopColor", "textAnchor", "fontSize", "fontFamily", "gradientUnits" ]


{-| `Html.Attributes` taking a `Bool`, rendered as the bare attribute when true. -}
htmlBoolAttrs : List String
htmlBoolAttrs =
    [ "checked", "disabled", "selected", "readonly", "autofocus", "hidden", "multiple" ]


{-| A value as the `String` it stringifies to — itself if already a string, else its rendered form
(for `String.join`/`String.concat` over non-string lists). -}
renderStr : Value -> String
renderStr v =
    case v of
        VStr s ->
            s

        _ ->
            renderValue v


renderValue : Value -> String
renderValue v =
    case v of
        VNum n ->
            String.fromFloat n

        VBool b ->
            if b then
                "True"

            else
                "False"

        VStr s ->
            "\"" ++ s ++ "\""

        VChar c ->
            "'" ++ String.fromChar c ++ "'"

        VList items ->
            "[" ++ String.join ", " (List.map renderValue items) ++ "]"

        VTup items ->
            "(" ++ String.join ", " (List.map renderValue items) ++ ")"

        VCtor "Dict" [ VList pairs ] ->
            "Dict.fromList [" ++ String.join "," (List.map renderValue pairs) ++ "]"

        VCtor "Set" [ VList elems ] ->
            "Set.fromList [" ++ String.join "," (List.map renderValue elems) ++ "]"

        VCtor "Array" [ VList elems ] ->
            "Array.fromList [" ++ String.join "," (List.map renderValue elems) ++ "]"

        VCtor name args ->
            if List.isEmpty args then
                name

            else
                name ++ " " ++ String.join " " (List.map renderValueAtom args)

        VRecord fields ->
            if List.isEmpty fields then
                "{}"

            else
                "{ " ++ String.join ", " (List.map (\f -> Tuple.first f ++ " = " ++ renderValue (Tuple.second f)) fields) ++ " }"

        VClosure _ _ _ ->
            "<function>"

        VRec _ _ _ _ ->
            "<function>"

        VBuiltin name _ ->
            "<" ++ name ++ ">"


renderValueAtom : Value -> String
renderValueAtom v =
    case v of
        VCtor _ args ->
            if List.isEmpty args then
                renderValue v

            else
                "(" ++ renderValue v ++ ")"

        _ ->
            renderValue v


htmlToString : Value -> String
htmlToString v =
    case v of
        VCtor "Html.text" [ VStr s ] ->
            s

        VCtor "Html.text" [ other ] ->
            renderValue other

        VCtor "Html.node" [ VStr tag, VList attrs, VList children ] ->
            "<" ++ tag ++ attrsToString attrs ++ ">" ++ String.concat (List.map htmlToString children) ++ "</" ++ tag ++ ">"

        _ ->
            renderValue v


attrsToString : List Value -> String
attrsToString attrs =
    String.concat (List.map attrToString attrs)


attrToString : Value -> String
attrToString v =
    case v of
        VCtor "Html.on" [ VStr ev, msg ] ->
            " on" ++ ev ++ "=" ++ renderValue msg

        VCtor "Html.style" [ VStr k, VStr val ] ->
            " style=" ++ k ++ ":" ++ val

        VCtor "Html.attr" [ VStr k, VStr val ] ->
            " " ++ k ++ "=" ++ val

        VCtor "Html.attr" [ VStr k, VBool b ] ->
            if b then
                " " ++ k

            else
                ""

        VCtor "Html.attr" [ VStr k, other ] ->
            " " ++ k ++ "=" ++ renderValue other

        _ ->
            ""


{-| Maps an attribute builtin name to its rendered key (`type_` is a keyword-avoiding alias). -}
attrKey : String -> String
attrKey name =
    if name == "type_" then
        "type"

    else if name == "strokeWidth" then
        "stroke-width"

    else if name == "strokeLinecap" then
        "stroke-linecap"

    else if name == "strokeDasharray" then
        "stroke-dasharray"

    else if name == "fillOpacity" then
        "fill-opacity"

    else if name == "stopColor" then
        "stop-color"

    else if name == "textAnchor" then
        "text-anchor"

    else if name == "fontSize" then
        "font-size"

    else if name == "fontFamily" then
        "font-family"

    else if name == "gradientUnits" then
        "gradientUnits"

    else
        name
