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
import Set exposing (Set)


{-| The Html (and Svg) element/attribute/event builtins, as a {@link Eval.Core.Processor}. They build
the `Html.node`/`Html.attr`/`Html.text`/`Html.on`/`Html.style` value trees `htmlToString` renders. -}
processor : Processor
processor =
    { names = htmlTags ++ htmlStringAttrs ++ htmlBoolAttrs ++ [ "text", "onClick", "onInput", "onCheck", "onSubmit", "onDoubleClick", "onMouseDown", "onMouseUp", "onMouseEnter", "onMouseLeave", "onMouseOver", "onMouseOut", "onFocus", "onBlur", "on", "preventDefaultOn", "stopPropagationOn", "style" ]
    , arities = [ ( 1, htmlStringAttrs ++ htmlBoolAttrs ++ [ "text", "onClick", "onInput", "onCheck", "onSubmit", "onDoubleClick", "onMouseDown", "onMouseUp", "onMouseEnter", "onMouseLeave", "onMouseOver", "onMouseOut", "onFocus", "onBlur" ] ) ]
    , run = run
    }


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    if Set.member name htmlTagSet then
        case args of
            [ attrs, children ] ->
                Just (Ok (VCtor "Html.node" [ VStr (tagName name), attrs, children ]))

            _ ->
                Just (Err (name ++ " needs attributes and children"))

    else if Set.member name htmlStringAttrSet || Set.member name htmlBoolAttrSet then
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

            -- The remaining Html.Events helpers, rendered as inert handlers (the editor wires only
            -- click/input live); each maps to its DOM event name.
            ( "onCheck", [ handler ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "change", handler ]))

            ( "onSubmit", [ msg ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "submit", msg ]))

            ( "onDoubleClick", [ msg ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "dblclick", msg ]))

            ( "onMouseDown", [ msg ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "mousedown", msg ]))

            ( "onMouseUp", [ msg ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "mouseup", msg ]))

            ( "onMouseEnter", [ msg ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "mouseenter", msg ]))

            ( "onMouseLeave", [ msg ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "mouseleave", msg ]))

            ( "onMouseOver", [ msg ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "mouseover", msg ]))

            ( "onMouseOut", [ msg ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "mouseout", msg ]))

            ( "onFocus", [ msg ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "focus", msg ]))

            ( "onBlur", [ msg ] ) ->
                Just (Ok (VCtor "Html.on" [ VStr "blur", msg ]))

            ( "style", [ k, v ] ) ->
                Just (Ok (VCtor "Html.style" [ k, v ]))

            _ ->
                Nothing


{-| Membership sets for the tag/attribute lists, built once, so `run` resolves an element or
attribute name with an O(log n) `Set.member` instead of scanning the ~120-entry lists per node. -}
htmlTagSet : Set String
htmlTagSet =
    Set.fromList htmlTags


htmlStringAttrSet : Set String
htmlStringAttrSet =
    Set.fromList htmlStringAttrs


htmlBoolAttrSet : Set String
htmlBoolAttrSet =
    Set.fromList htmlBoolAttrs


{-| The Html/Svg element tags that build a `Html.node` (`circle` here is the SVG circle; the
playground `circle` is disambiguated by Eval.Playground). -}
htmlTags : List String
htmlTags =
    -- The full Html element set (matching the elm-lang interpreter's Prelude.HTML_TAGS), so anything
    -- that runs there renders the same in the editor's preview. Reserved-word aliases (main_/var_/
    -- object_) render via `tagName`.
    [ "div", "span", "p", "h1", "h2", "h3", "h4", "h5", "h6", "ul", "ol", "li", "a", "img", "button", "input", "label", "form", "section", "header", "footer", "nav", "main_", "br", "hr", "table", "thead", "tbody", "tr", "td", "th", "pre", "code", "strong", "em", "i", "b", "small", "select", "option", "textarea", "canvas", "audio", "video", "fieldset", "legend", "figure", "blockquote", "cite", "figcaption", "caption", "abbr", "address", "article", "aside", "details", "summary", "mark", "time", "u", "s", "sub", "sup", "kbd", "samp", "var_", "dl", "dt", "dd", "menu", "progress", "meter", "output", "datalist", "iframe", "embed", "object_" ]
        ++ [ "colgroup", "col", "tfoot", "optgroup", "source", "track", "param", "ins", "del", "dfn", "q", "ruby", "rt", "rp", "bdi", "bdo", "wbr", "menuitem", "math" ]
        -- `style` and `title` are intentionally omitted: the editor dispatches Html/Svg by unqualified
        -- name and resolves elements before attributes, so listing them as SVG elements would shadow
        -- the Html.Attributes.style / Html.Attributes.title attributes. The main backends qualify the
        -- names (Svg.style vs Html.Attributes.style) and so include them.
        ++ [ "svg", "foreignObject", "circle", "ellipse", "image", "line", "path", "polygon", "polyline", "rect", "use", "defs", "g", "marker", "mask", "pattern", "switch", "symbol", "clipPath", "cursor", "filter", "view", "desc", "metadata", "linearGradient", "radialGradient", "stop", "text_", "textPath", "tref", "tspan", "altGlyph", "altGlyphDef", "altGlyphItem", "glyph", "glyphRef", "font", "colorProfile", "animate", "animateColor", "animateMotion", "animateTransform", "mpath", "set", "feBlend", "feColorMatrix", "feComponentTransfer", "feComposite", "feConvolveMatrix", "feDiffuseLighting", "feDisplacementMap", "feFlood", "feFuncA", "feFuncB", "feFuncG", "feFuncR", "feGaussianBlur", "feImage", "feMerge", "feMergeNode", "feMorphology", "feOffset", "feSpecularLighting", "feTile", "feTurbulence", "feDistantLight", "fePointLight", "feSpotLight" ]


{-| The rendered tag for an element builtin: the reserved-word-avoiding `_` aliases map to their real
HTML tag (`main_` → `main`, etc.); every other name is its own tag. -}
tagName : String -> String
tagName name =
    case name of
        "main_" ->
            "main"

        "var_" ->
            "var"

        "object_" ->
            "object"

        "text_" ->
            "text"

        "colorProfile" ->
            "color-profile"

        _ ->
            name


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
