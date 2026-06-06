module Highlight exposing (segments, cssSegments)

{-| A character-faithful syntax highlighter for the editor: turns Elm source into a list of
`(class, text)` segments whose texts, concatenated, reproduce the input exactly (including every
space and newline). The editor renders each segment as a coloured `<span>` in a `<pre>` sitting
behind a transparent `<textarea>`, so the caret and the colours stay aligned. Classes are:

  - `"kw"`  keywords (`module`, `let`, `case`, …)
  - `"type"` upper-case names (types and constructors)
  - `"num"` number literals
  - `"str"` string and character literals
  - `"com"` line and block comments
  - `"op"`  operators and punctuation
  - `""`    everything else (identifiers, whitespace)

There is also a `cssSegments` highlighter (same `(class, text)` contract) for editing CSS — used by
embedders like the Bootstrap theme builder that reuse the editor widget over a CSS document rather
than Elm.

-}


keywords : List String
keywords =
    [ "module", "import", "exposing", "type", "alias", "port", "infix"
    , "let", "in", "if", "then", "else", "case", "of", "as", "where"
    ]


{-| Segments the source into `(class, text)` pairs in order; concatenating the texts yields `src`. -}
segments : String -> List ( String, String )
segments src =
    walk (String.toList src) []


walk : List Char -> List ( String, String ) -> List ( String, String )
walk chars acc =
    case chars of
        [] ->
            List.reverse acc

        c :: rest ->
            if c == '-' && startsWith2 '-' rest then
                -- A line comment: `--` to the end of the line (`rest` still holds the second dash).
                let
                    ( body, after ) =
                        spanWhile (\ch -> ch /= '\n') (List.drop 1 rest)
                in
                walk after (( "com", "--" ++ body ) :: acc)

            else if c == '{' && startsWith2 '-' rest then
                -- A block comment `{- … -}` (closing scanned non-nested, which suffices for colours).
                let
                    ( body, after ) =
                        blockComment (List.drop 1 rest) "{-"
                in
                walk after (( "com", body ) :: acc)

            else if c == '"' then
                let
                    ( body, after ) =
                        stringLit rest "\""
                in
                walk after (( "str", body ) :: acc)

            else if c == '\'' then
                let
                    ( body, after ) =
                        charLit rest "'"
                in
                walk after (( "str", body ) :: acc)

            else if isSpace c then
                let
                    ( run, after ) =
                        spanWhile isSpace chars
                in
                walk after (( "", run ) :: acc)

            else if Char.isDigit c then
                let
                    ( run, after ) =
                        spanWhile isNumChar chars
                in
                walk after (( "num", run ) :: acc)

            else if Char.isAlpha c || c == '_' then
                let
                    ( run, after ) =
                        spanWhile isIdChar chars

                    cls =
                        if List.member run keywords then
                            "kw"

                        else if Char.isUpper c then
                            "type"

                        else
                            ""
                in
                walk after (( cls, run ) :: acc)

            else if isOpChar c then
                let
                    ( run, after ) =
                        spanWhile isOpChar chars
                in
                walk after (( "op", run ) :: acc)

            else
                -- Punctuation: parens, brackets, braces, comma, dot, etc.
                walk rest (( "op", String.fromChar c ) :: acc)


{-| True when the first char of `rest` equals `c` (used to spot the second char of a comment opener). -}
startsWith2 : Char -> List Char -> Bool
startsWith2 c rest =
    case rest of
        h :: _ ->
            h == c

        [] ->
            False


{-| Consumes the rest of a block comment up to and including its closing delimiter. -}
blockComment : List Char -> String -> ( String, List Char )
blockComment chars acc =
    case chars of
        '-' :: '}' :: rest ->
            ( acc ++ "-}", rest )

        c :: rest ->
            blockComment rest (acc ++ String.fromChar c)

        [] ->
            ( acc, [] )


{-| Consumes a string literal body (the opening quote is already in `acc`), honouring `\"`. -}
stringLit : List Char -> String -> ( String, List Char )
stringLit chars acc =
    case chars of
        '\\' :: c :: rest ->
            stringLit rest (acc ++ "\\" ++ String.fromChar c)

        '"' :: rest ->
            ( acc ++ "\"", rest )

        c :: rest ->
            stringLit rest (acc ++ String.fromChar c)

        [] ->
            ( acc, [] )


{-| Consumes a character literal `'c'` (or `'\n'`); the opening quote is already in `acc`. -}
charLit : List Char -> String -> ( String, List Char )
charLit chars acc =
    case chars of
        '\\' :: c :: '\'' :: rest ->
            ( acc ++ "\\" ++ String.fromChar c ++ "'", rest )

        c :: '\'' :: rest ->
            ( acc ++ String.fromChar c ++ "'", rest )

        c :: rest ->
            charLit rest (acc ++ String.fromChar c)

        [] ->
            ( acc, [] )


{-| Splits off the leading run of chars satisfying `pred`, returning `(run, remainder)`. -}
spanWhile : (Char -> Bool) -> List Char -> ( String, List Char )
spanWhile pred chars =
    spanHelp pred chars ""


spanHelp : (Char -> Bool) -> List Char -> String -> ( String, List Char )
spanHelp pred chars acc =
    case chars of
        c :: rest ->
            if pred c then
                spanHelp pred rest (acc ++ String.fromChar c)

            else
                ( acc, chars )

        [] ->
            ( acc, [] )


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}'


isNumChar : Char -> Bool
isNumChar c =
    Char.isDigit c || c == '.' || c == 'e' || c == 'x' || Char.isHexDigit c


isIdChar : Char -> Bool
isIdChar c =
    Char.isAlphaNum c || c == '_' || c == '.'


isOpChar : Char -> Bool
isOpChar c =
    c == '+' || c == '-' || c == '*' || c == '/' || c == '=' || c == '<' || c == '>' || c == '&' || c == '|' || c == ':' || c == '^' || c == '%' || c == '\\'



-- CSS


{-| A character-faithful CSS highlighter (same `(class, text)` contract as `segments`): concatenating
the texts reproduces the input exactly. It is brace-aware so it can tell a **selector** (outside a
`{ … }` block) from a **property** (inside a block, before the `:`) from a **value** (after the `:`).
Classes are:

  - `"com"`  `/* … */` comments
  - `"str"`  string literals
  - `"num"`  numbers and `#hex` colours
  - `"var"`  custom properties / `--variables`
  - `"kw"`   at-rules (`@media`, `@import`, …)
  - `"sel"`  selectors (at brace depth 0)
  - `"prop"` property names (inside a block, before the colon)
  - `"val"`  value identifiers (inside a block, after the colon)
  - `"op"`   braces, colons, semicolons and other punctuation
  - `""`     whitespace

-}
cssSegments : String -> List ( String, String )
cssSegments src =
    cssWalk (String.toList src) 0 False []


{-| The stateful walk: `depth` is the current `{ }` nesting and `inValue` is True once a `:` has been
seen in the current declaration (reset by `;`, `{` or `}`), which is what distinguishes a property
name from its value. -}
cssWalk : List Char -> Int -> Bool -> List ( String, String ) -> List ( String, String )
cssWalk chars depth inValue acc =
    case chars of
        [] ->
            List.reverse acc

        c :: rest ->
            if c == '/' && startsWith2 '*' rest then
                let
                    ( body, after ) =
                        cssComment (List.drop 1 rest) "/*"
                in
                cssWalk after depth inValue (( "com", body ) :: acc)

            else if c == '"' || c == '\'' then
                let
                    ( body, after ) =
                        cssString c rest (String.fromChar c)
                in
                cssWalk after depth inValue (( "str", body ) :: acc)

            else if isSpace c then
                let
                    ( run, after ) =
                        spanWhile isSpace chars
                in
                cssWalk after depth inValue (( "", run ) :: acc)

            else if c == '{' then
                cssWalk rest (depth + 1) False (( "op", "{" ) :: acc)

            else if c == '}' then
                cssWalk rest (max 0 (depth - 1)) False (( "op", "}" ) :: acc)

            else if c == ';' then
                cssWalk rest depth False (( "op", ";" ) :: acc)

            else if c == ':' then
                -- Entering value position (only meaningful inside a declaration block).
                cssWalk rest depth (depth > 0) (( "op", ":" ) :: acc)

            else if c == '#' then
                let
                    ( run, after ) =
                        spanWhile Char.isAlphaNum rest
                in
                cssWalk after depth inValue (( "num", "#" ++ run ) :: acc)

            else if c == '@' then
                let
                    ( run, after ) =
                        spanWhile isCssIdent rest
                in
                cssWalk after depth inValue (( "kw", "@" ++ run ) :: acc)

            else if c == '-' && startsWith2 '-' rest then
                -- A custom property / CSS variable: `--foo`.
                let
                    ( run, after ) =
                        spanWhile isCssIdent (List.drop 1 rest)
                in
                cssWalk after depth inValue (( "var", "--" ++ run ) :: acc)

            else if Char.isDigit c then
                let
                    ( run, after ) =
                        spanWhile (\ch -> Char.isDigit ch || ch == '.') chars
                in
                cssWalk after depth inValue (( "num", run ) :: acc)

            else if Char.isAlpha c || c == '_' then
                let
                    ( run, after ) =
                        spanWhile isCssIdent chars

                    cls =
                        if depth == 0 then
                            "sel"

                        else if inValue then
                            "val"

                        else
                            "prop"
                in
                cssWalk after depth inValue (( cls, run ) :: acc)

            else
                -- Punctuation: combinators (`>`, `~`, `+`), `,`, `(`, `)`, `*`, `!`, etc.
                cssWalk rest depth inValue (( "op", String.fromChar c ) :: acc)


{-| Consumes the rest of a CSS block comment up to and including its closing `*/`. -}
cssComment : List Char -> String -> ( String, List Char )
cssComment chars acc =
    case chars of
        '*' :: '/' :: rest ->
            ( acc ++ "*/", rest )

        c :: rest ->
            cssComment rest (acc ++ String.fromChar c)

        [] ->
            ( acc, [] )


{-| Consumes a quoted CSS string up to its matching `quote` (honouring `\` escapes); the opening
quote is already in `acc`. -}
cssString : Char -> List Char -> String -> ( String, List Char )
cssString quote chars acc =
    case chars of
        '\\' :: c :: rest ->
            cssString quote rest (acc ++ "\\" ++ String.fromChar c)

        c :: rest ->
            if c == quote then
                ( acc ++ String.fromChar c, rest )

            else
                cssString quote rest (acc ++ String.fromChar c)

        [] ->
            ( acc, [] )


{-| Characters that may appear inside a CSS identifier / property / variable name. -}
isCssIdent : Char -> Bool
isCssIdent c =
    Char.isAlphaNum c || c == '-' || c == '_'
