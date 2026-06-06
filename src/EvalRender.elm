module EvalRender exposing (attrKey, htmlToString, renderValue)

{-| Display helpers for the editor's interpreter: turning an interpreted `Value` (and the Html-value
trees the evaluator builds) into the text the REPL path and the result pane show. These are pure
`Value -> String` renderers with no dependency on evaluation, so they live apart from `Eval`'s
mutually-recursive core; `Eval` re-exposes `renderValue` and calls `htmlToString`/`attrKey`. -}

import Lang exposing (Value(..))


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
