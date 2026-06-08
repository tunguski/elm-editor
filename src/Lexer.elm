module Lexer exposing (Token(..), tokenize, cookLayout, collapseMultiline)

{-| The tokenizer for the interpreted language: turns source text into a flat list of tokens. It
keeps layout by emitting a `TLine indent` marker at the start of each non-blank line; `cookLayout`
then turns indentation into the `;` branch separators the parser expects (so real Elm `case`
expressions, which use layout rather than explicit separators, parse correctly). -}


type Token
    = TNum Float
    | TStr String
    | TChar Char
    | TId String
    | TUpper String
    | TOp String
    | TLParen
    | TRParen
    | TLBracket
    | TRBracket
    | TLBrace
    | TRBrace
    | TDot
    | TPipe
    | TComma
    | TSemi
    | TLambda
    | TArrow
    | TEquals
    | TLine Int


tokenize : String -> Result String (List Token)
tokenize src =
    tokenizeLines (String.lines (collapseMultiline src)) []


{-| Collapses the multi-line literals (`"""…"""` triple strings, `[glsl| … |]` shaders) into
single-line forms. `Parser.parseModule` applies this *before* splitting a module into top-level
declarations by line, so a triple string whose continuation lines start at column 0 (e.g.
`x = """a\nb"""`) isn't mistaken for the start of a new declaration. Idempotent — `tokenize` re-runs
it harmlessly once each chunk reaches the lexer. -}
collapseMultiline : String -> String
collapseMultiline src =
    inlineShaders (inlineTripleStrings src)


{-| Collapses each triple-quoted string `"""…"""` into an ordinary single-line `"…"` literal so the
line-based tokenizer can handle it: the body's newlines become `\n` escapes (and existing backslashes
/ quotes are escaped first), which the string lexer then decodes back — preserving the content. -}
inlineTripleStrings : String -> String
inlineTripleStrings src =
    case String.indexes "\"\"\"" src of
        [] ->
            src

        start :: _ ->
            let
                before =
                    String.left start src

                afterOpen =
                    String.dropLeft (start + 3) src
            in
            case String.indexes "\"\"\"" afterOpen of
                [] ->
                    src

                close :: _ ->
                    let
                        body =
                            String.left close afterOpen

                        rest =
                            String.dropLeft (close + 3) afterOpen

                        escaped =
                            escapeTripleBody (String.toList body) ""
                    in
                    before ++ "\"" ++ escaped ++ "\"" ++ inlineTripleStrings rest


{-| Re-escapes a triple-quoted string's body into the equivalent ordinary single-line `"…"` literal,
which the string lexer ({@link takeString}) then decodes. A triple string is processed like a normal
string except that *raw* double-quotes and newlines are allowed, so: raw `"` → `\"`, raw newline →
`\n`, raw CR dropped, and any existing escape sequence (`\n`, `\t`, `\\`, `\"`, `\u{…}`, …) is passed
through verbatim — crucially NOT re-escaped, which a blanket `\` → `\\` would do (turning the user's
`\n` into a literal backslash-n). -}
escapeTripleBody : List Char -> String -> String
escapeTripleBody chars acc =
    case chars of
        [] ->
            acc

        '\u{000D}' :: rest ->
            escapeTripleBody rest acc

        '\n' :: rest ->
            escapeTripleBody rest (acc ++ "\\n")

        '"' :: rest ->
            escapeTripleBody rest (acc ++ "\\\"")

        '\\' :: e :: rest ->
            -- An escape sequence: keep both chars as-is so the string lexer decodes it later.
            escapeTripleBody rest (acc ++ String.fromList [ '\\', e ])

        '\\' :: [] ->
            -- A trailing lone backslash (malformed); escape it so it can't swallow the closing quote.
            acc ++ "\\\\"

        c :: rest ->
            escapeTripleBody rest (acc ++ String.fromChar c)


{-| Collapses each multi-line GLSL shader literal `[glsl| … |]` into a single-line string literal so
the line-based tokenizer can handle it (the interpreter models a shader as its source string). The
body's newlines become spaces and embedded quotes are dropped — enough for the interpreter to
evaluate WebGL programs and report a scene preview (the JS backend does the actual GPU rendering). -}
inlineShaders : String -> String
inlineShaders src =
    case String.indexes "[glsl|" src of
        [] ->
            src

        start :: _ ->
            let
                before =
                    String.left start src

                afterOpen =
                    String.dropLeft (start + 6) src
            in
            case String.indexes "|]" afterOpen of
                [] ->
                    src

                close :: _ ->
                    let
                        body =
                            String.left close afterOpen

                        rest =
                            String.dropLeft (close + 2) afterOpen

                        flat =
                            String.replace "\"" "" (String.replace "\n" " " (String.replace "\u{000D}" " " body))
                    in
                    before ++ "\"" ++ flat ++ "\"" ++ inlineShaders rest


{-| Tokenizes line by line, prefixing each non-blank line's tokens with its indentation marker. -}
tokenizeLines : List String -> List Token -> Result String (List Token)
tokenizeLines lines acc =
    case lines of
        [] ->
            Ok acc

        line :: rest ->
            if String.trim line == "" then
                tokenizeLines rest acc

            else
                case tokenizeHelp (String.toList line) [] of
                    Ok toks ->
                        tokenizeLines rest (acc ++ (TLine (indentOf line) :: toks))

                    Err e ->
                        Err e


indentOf : String -> Int
indentOf line =
    String.length line - String.length (String.trimLeft line)


{-| Resolves layout: drops the `TLine` markers, inserting a `TSemi` between sibling `case` branches
and `let` bindings (lines at the branch/binding indentation). Operates per top-level chunk, so the
only column-0 line is the chunk header — branch indentation is always deeper, which keeps the rule
simple and safe. -}
cookLayout : List Token -> List Token
cookLayout toks =
    cook toks [] [] False


cook : List Token -> List Token -> List Int -> Bool -> List Token
cook toks out stack afterOf =
    case toks of
        [] ->
            List.reverse out

        (TLine col) :: rest ->
            if afterOf then
                cook rest out (col :: stack) False

            else
                let
                    popped =
                        dropWhileGreater col stack
                in
                case popped of
                    h :: _ ->
                        if h == col then
                            case out of
                                TSemi :: _ ->
                                    cook rest out popped False

                                _ ->
                                    cook rest (TSemi :: out) popped False

                        else
                            cook rest out popped False

                    [] ->
                        cook rest out popped False

        (TId "of") :: rest ->
            cook rest (TId "of" :: out) stack True

        (TId "let") :: rest ->
            -- like `of`: the next line establishes the binding column, so sibling
            -- bindings at that column get a `TSemi`; the dedent at `in` pops it back.
            cook rest (TId "let" :: out) stack True

        t :: rest ->
            cook rest (t :: out) stack afterOf


dropWhileGreater : Int -> List Int -> List Int
dropWhileGreater col stack =
    case stack of
        h :: rest ->
            if h > col then
                dropWhileGreater col rest

            else
                stack

        [] ->
            []


{-| Reads a negative number literal from chars beginning with `-` immediately followed by a digit
(e.g. `-2`, `-1.5`), returning the token and the rest. Used for prefix negation in argument
position; ordinary `a - b` keeps the spaces around `-` so this does not fire. -}
negNumber : List Char -> Maybe ( Token, List Char )
negNumber chars =
    case chars of
        '-' :: d :: _ ->
            if Char.isDigit d then
                readNumber (List.drop 1 chars)
                    |> Maybe.map (\( n, after ) -> ( TNum (negate n), after ))

            else
                Nothing

        _ ->
            Nothing


{-| Whether a `-` here begins an expression (so a following number is negated), based on the
preceding token: start of input, an opener (`(`/`[`/`,`/`=`/`->`/`\`) or another operator. -}
prefixContext : List Token -> Bool
prefixContext acc =
    case acc of
        [] ->
            True

        (TOp _) :: _ ->
            True

        t :: _ ->
            List.member t [ TLParen, TLBracket, TComma, TEquals, TArrow, TLambda ]


tokenizeHelp : List Char -> List Token -> Result String (List Token)
tokenizeHelp chars acc =
    case chars of
        [] ->
            Ok (List.reverse acc)

        c :: rest ->
            if c == ' ' || c == '\n' || c == '\t' || c == '\u{000D}' then
                -- After whitespace, a `-` glued to a digit (`f -2`) is a negative literal, not a minus.
                case negNumber rest of
                    Just ( tok, after ) ->
                        tokenizeHelp after (tok :: acc)

                    Nothing ->
                        tokenizeHelp rest acc

            else if c == '-' && List.head rest == Just '-' then
                -- line comment: drop the rest of the line
                Ok (List.reverse acc)

            else if c == '-' && prefixContext acc then
                -- A `-` after `(`, `[`, `,`, `=`, `->` or another operator negates a following number.
                case negNumber (c :: rest) of
                    Just ( tok, after ) ->
                        tokenizeHelp after (tok :: acc)

                    Nothing ->
                        tokenizeHelp rest (TOp "-" :: acc)

            else if c == '(' then
                tokenizeHelp rest (TLParen :: acc)

            else if c == ')' then
                tokenizeHelp rest (TRParen :: acc)

            else if c == '[' then
                tokenizeHelp rest (TLBracket :: acc)

            else if c == ']' then
                tokenizeHelp rest (TRBracket :: acc)

            else if c == '{' then
                tokenizeHelp rest (TLBrace :: acc)

            else if c == '}' then
                tokenizeHelp rest (TRBrace :: acc)

            else if c == '.' then
                tokenizeHelp rest (TDot :: acc)

            else if c == ',' then
                tokenizeHelp rest (TComma :: acc)

            else if c == ';' then
                tokenizeHelp rest (TSemi :: acc)

            else if c == '\\' then
                tokenizeHelp rest (TLambda :: acc)

            else if c == '"' then
                -- Triple-quoted strings are flattened to ordinary `"…"` literals by
                -- inlineTripleStrings before tokenizing, so only single-quoted strings reach here.
                let
                    taken =
                        takeString rest ""
                in
                tokenizeHelp (Tuple.second taken) (TStr (Tuple.first taken) :: acc)

            else if c == '\'' then
                case takeChar rest of
                    Just ( ch, after ) ->
                        tokenizeHelp after (TChar ch :: acc)

                    Nothing ->
                        Err "bad character literal"

            else if isOpChar c then
                let
                    taken =
                        takeWhile isOpChar chars ""
                in
                case classifyOp (Tuple.first taken) of
                    Ok tok ->
                        tokenizeHelp (Tuple.second taken) (tok :: acc)

                    Err e ->
                        Err e

            else if Char.isDigit c then
                case readNumber chars of
                    Just ( n, after ) ->
                        tokenizeHelp after (TNum n :: acc)

                    Nothing ->
                        Err ("bad number near: " ++ String.fromList (List.take 8 chars))

            else if Char.isAlpha c || c == '_' then
                let
                    taken =
                        takeWhile isIdChar chars ""

                    word =
                        Tuple.first taken

                    token =
                        if Char.isUpper c then
                            TUpper word

                        else
                            TId word
                in
                tokenizeHelp (Tuple.second taken) (token :: acc)

            else
                Err ("unexpected character: " ++ String.fromChar c)


isOpChar : Char -> Bool
isOpChar c =
    c == '+' || c == '-' || c == '*' || c == '/' || c == '=' || c == '<' || c == '>' || c == '&' || c == '|' || c == ':' || c == '^'


isNumChar : Char -> Bool
isNumChar c =
    Char.isDigit c || c == '.'


isHexChar : Char -> Bool
isHexChar c =
    Char.isDigit c || (Char.toLower c >= 'a' && Char.toLower c <= 'f')


{-| Reads a numeric literal: hexadecimal `0x…`, or a decimal with an optional fraction and an
optional `eE[+-]?digits` exponent (`42`, `1.5`, `2e9`, `1.5e-3`). Returns the value and the rest. -}
readNumber : List Char -> Maybe ( Float, List Char )
readNumber chars =
    case chars of
        '0' :: x :: hd :: rest ->
            if (x == 'x' || x == 'X') && isHexChar hd then
                let
                    ( hex, after ) =
                        takeWhile isHexChar (hd :: rest) ""
                in
                Just ( toFloat (hexToInt hex), after )

            else
                readDecimal chars

        _ ->
            readDecimal chars


readDecimal : List Char -> Maybe ( Float, List Char )
readDecimal chars =
    let
        ( mant, rest1 ) =
            takeWhile isNumChar chars ""

        ( expPart, rest2 ) =
            readExponent rest1
    in
    String.toFloat (mant ++ expPart) |> Maybe.map (\n -> ( n, rest2 ))


{-| An optional `eE[+-]?digits` exponent suffix; "" (and the unchanged input) when there is none. -}
readExponent : List Char -> ( String, List Char )
readExponent chars =
    case chars of
        e :: sign :: d :: rest ->
            if (e == 'e' || e == 'E') && (sign == '+' || sign == '-') && Char.isDigit d then
                let
                    ( ds, after ) =
                        takeWhile Char.isDigit (d :: rest) ""
                in
                ( String.fromChar e ++ String.fromChar sign ++ ds, after )

            else
                readExponentNoSign chars

        _ ->
            readExponentNoSign chars


readExponentNoSign : List Char -> ( String, List Char )
readExponentNoSign chars =
    case chars of
        e :: d :: rest ->
            if (e == 'e' || e == 'E') && Char.isDigit d then
                let
                    ( ds, after ) =
                        takeWhile Char.isDigit (d :: rest) ""
                in
                ( String.fromChar e ++ ds, after )

            else
                ( "", chars )

        _ ->
            ( "", chars )


isIdChar : Char -> Bool
isIdChar c =
    Char.isAlphaNum c || c == '_'


takeWhile : (Char -> Bool) -> List Char -> String -> ( String, List Char )
takeWhile pred chars acc =
    case chars of
        c :: rest ->
            if pred c then
                takeWhile pred rest (acc ++ String.fromChar c)

            else
                ( acc, chars )

        [] ->
            ( acc, chars )


{-| Reads a character literal `'c'` (or an escape like `'\n'`), given the opening `'` was consumed.
Returns the character and the input after the closing `'`, or `Nothing` if it is malformed. -}
takeChar : List Char -> Maybe ( Char, List Char )
takeChar chars =
    case chars of
        '\\' :: 'u' :: '{' :: rest ->
            -- A Unicode escape `'\u{HHHH}'`.
            let
                ( hex, afterHex ) =
                    takeWhile (\c -> c /= '}') rest ""
            in
            case afterHex of
                '}' :: '\'' :: more ->
                    Just ( Char.fromCode (hexToInt hex), more )

                _ ->
                    Nothing

        '\\' :: e :: '\'' :: rest ->
            Just ( unescapeChar e, rest )

        c :: '\'' :: rest ->
            Just ( c, rest )

        _ ->
            Nothing


unescapeChar : Char -> Char
unescapeChar e =
    case e of
        'n' ->
            '\n'

        't' ->
            '\t'

        'r' ->
            '\u{000D}'

        _ ->
            e


takeString : List Char -> String -> ( String, List Char )
takeString chars acc =
    case chars of
        '"' :: rest ->
            ( acc, rest )

        '\\' :: 'u' :: '{' :: rest ->
            -- A Unicode escape `\u{HHHH}`: read hex digits up to the closing brace.
            let
                ( hex, afterHex ) =
                    takeWhile (\c -> c /= '}') rest ""
            in
            case afterHex of
                '}' :: more ->
                    takeString more (acc ++ String.fromChar (Char.fromCode (hexToInt hex)))

                _ ->
                    takeString rest (acc ++ "\\u{")

        '\\' :: e :: rest ->
            -- A backslash escape (`\n`, `\t`, `\r`, `\\`, `\"`, …); unescapeChar leaves others as-is.
            takeString rest (acc ++ String.fromChar (unescapeChar e))

        c :: rest ->
            takeString rest (acc ++ String.fromChar c)

        [] ->
            ( acc, [] )


{-| Parses a string of hex digits to an Int (case-insensitive); unknown digits count as 0. -}
hexToInt : String -> Int
hexToInt hex =
    String.foldl (\c acc -> acc * 16 + hexDigit c) 0 hex


hexDigit : Char -> Int
hexDigit c =
    let
        code =
            Char.toCode (Char.toLower c)
    in
    if code >= 48 && code <= 57 then
        code - 48

    else if code >= 97 && code <= 102 then
        code - 87

    else
        0


classifyOp : String -> Result String Token
classifyOp s =
    if s == "->" then
        Ok TArrow

    else if s == "=" then
        Ok TEquals

    else if s == "|" then
        Ok TPipe

    else if s == ":" then
        -- A lone colon: a type annotation marker. Lexing it (rather than erroring) lets the parser
        -- skip `name : Type` annotations inside `let`; `::` (cons) is lexed as its own op above.
        Ok (TOp ":")

    else if List.member s [ "+", "-", "*", "/", "//", "^", "==", "/=", "<", "<=", ">", ">=", "&&", "||", "++", "::", "|>", "<|", ">>", "<<" ] then
        Ok (TOp s)

    else
        Err ("unknown operator: " ++ s)
