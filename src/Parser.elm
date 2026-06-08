module Parser exposing (parse, parseProject)

{-| Parses tokens into expressions (precedence-climbing) and a project's source files into a
mutually-recursive set of top-level declarations (a column-0 "layout-lite" chunker). -}

import Dict
import Lang exposing (Decl, Expr(..), Globals, Pattern(..))
import Lexer exposing (Token(..), collapseMultiline, cookLayout, tokenize)



-- EXPRESSION PARSER (precedence climbing)


parse : List Token -> Result String Expr
parse rawTokens =
    parseExpr (cookLayout rawTokens)
        |> Result.andThen
            (\r ->
                if List.isEmpty (Tuple.second r) then
                    Ok (Tuple.first r)

                else
                    Err "unexpected trailing input"
            )


parseExpr : List Token -> Result String ( Expr, List Token )
parseExpr tokens =
    parseBinary 0 tokens


opPrec : String -> Int
opPrec op =
    if op == "|>" || op == "<|" then
        1

    else if op == "||" then
        2

    else if op == "&&" then
        3

    else if List.member op [ "==", "/=", "<", "<=", ">", ">=" ] then
        4

    else if op == "++" || op == "::" then
        5

    else if op == "+" || op == "-" then
        6

    else if op == "*" || op == "/" || op == "//" then
        7

    else if op == ">>" || op == "<<" then
        8

    else
        9


parseBinary : Int -> List Token -> Result String ( Expr, List Token )
parseBinary minPrec tokens =
    parseUnary tokens
        |> Result.andThen (\r -> climb minPrec (Tuple.first r) (Tuple.second r))


climb : Int -> Expr -> List Token -> Result String ( Expr, List Token )
climb minPrec left tokens =
    case tokens of
        (TOp op) :: rest ->
            if opPrec op >= minPrec then
                -- Right-associative operators (`<|`, `::`, `++`, `^`, `&&`, `||`) recurse at their own
                -- precedence so `a <| b <| c` groups as `a <| (b <| c)`; left-associative ones use
                -- prec+1. (Getting `<|` right is what makes `f <| g <| x` mean `f (g x)`.)
                let
                    nextMin =
                        if rightAssoc op then
                            opPrec op

                        else
                            opPrec op + 1
                in
                parseBinary nextMin rest
                    |> Result.andThen (\r -> climb minPrec (mkBin op left (Tuple.first r)) (Tuple.second r))

            else
                Ok ( left, tokens )

        _ ->
            Ok ( left, tokens )


{-| The right-associative operators (Elm's `infixr`). Left-associative ones (`|>`, `+`, `*`, …) are
the default. Associativity only changes the result for `<|`, `::` and `^`; the others are listed for
correctness. -}
rightAssoc : String -> Bool
rightAssoc op =
    List.member op [ "<|", "::", "++", "^", "&&", "||" ]


{-| Pipe and composition operators desugar into ordinary application / lambdas so the
evaluator needs no special cases: `a |> f` = `f a`, `f <| a` = `f a`,
`f >> g` = `\x -> g (f x)`, `f << g` = `\x -> f (g x)`.
-}
mkBin : String -> Expr -> Expr -> Expr
mkBin op left right =
    if op == "|>" then
        App right left

    else if op == "<|" then
        App left right

    else if op == ">>" then
        Lam [ "$x" ] (App right (App left (Var "$x")))

    else if op == "<<" then
        Lam [ "$x" ] (App left (App right (Var "$x")))

    else
        BinOp op left right


parseUnary : List Token -> Result String ( Expr, List Token )
parseUnary tokens =
    case tokens of
        (TOp "-") :: rest ->
            parseUnary rest |> Result.map (\r -> ( Neg (Tuple.first r), Tuple.second r ))

        _ ->
            parseApp tokens


parseApp : List Token -> Result String ( Expr, List Token )
parseApp tokens =
    parseAccess tokens
        |> Result.andThen (\r -> appTail (Tuple.first r) (Tuple.second r))


appTail : Expr -> List Token -> Result String ( Expr, List Token )
appTail fn tokens =
    if startsAtom tokens then
        parseAccess tokens
            |> Result.andThen (\r -> appTail (App fn (Tuple.first r)) (Tuple.second r))

    else
        Ok ( fn, tokens )


{-| An atom followed by zero or more `.field` accesses (`record.field.sub`). -}
parseAccess : List Token -> Result String ( Expr, List Token )
parseAccess tokens =
    parseAtom tokens
        |> Result.andThen (\r -> accessTail (Tuple.first r) (Tuple.second r))


accessTail : Expr -> List Token -> Result String ( Expr, List Token )
accessTail e tokens =
    case tokens of
        TDot :: (TId field) :: rest ->
            accessTail (RecordGet e field) rest

        TDot :: (TUpper seg) :: rest ->
            -- A nested module segment of a qualified name (e.g. `File.Select.file`): fold it into the
            -- module Ctor so the final `.lower` resolves to the builtin `File.Select.file`.
            case e of
                Ctor m ->
                    accessTail (Ctor (m ++ "." ++ seg)) rest

                _ ->
                    Ok ( e, tokens )

        _ ->
            Ok ( e, tokens )


startsAtom : List Token -> Bool
startsAtom tokens =
    case tokens of
        (TNum _) :: _ ->
            True

        (TStr _) :: _ ->
            True

        (TChar _) :: _ ->
            True

        TLParen :: _ ->
            True

        TLBracket :: _ ->
            True

        TLBrace :: _ ->
            True

        (TUpper _) :: _ ->
            True

        (TId name) :: _ ->
            not (List.member name [ "then", "else", "in", "of", "case" ])

        _ ->
            False


parseAtom : List Token -> Result String ( Expr, List Token )
parseAtom tokens =
    case tokens of
        (TNum n) :: rest ->
            Ok ( Num n, rest )

        (TStr s) :: rest ->
            Ok ( Str s, rest )

        (TChar ch) :: rest ->
            Ok ( CharLit ch, rest )

        (TUpper "True") :: rest ->
            Ok ( Boolean True, rest )

        (TUpper "False") :: rest ->
            Ok ( Boolean False, rest )

        (TUpper name) :: rest ->
            Ok ( Ctor name, rest )

        (TId "if") :: rest ->
            parseIf rest

        (TId "let") :: rest ->
            parseLet rest

        (TId "case") :: rest ->
            parseCase rest

        (TId name) :: rest ->
            Ok ( Var name, rest )

        TLambda :: rest ->
            parseLambda rest

        TLParen :: TRParen :: rest ->
            -- The unit value `()`, modelled as the empty tuple.
            Ok ( Tup [], rest )

        TLParen :: (TOp op) :: TRParen :: rest ->
            -- An operator used as a function, e.g. `(+)`, `(::)`, `(|>)` — `\a b -> a op b`.
            Ok ( Lam [ "$opl", "$opr" ] (mkBin op (Var "$opl") (Var "$opr")), rest )

        TLParen :: rest ->
            parseExpr rest
                |> Result.andThen
                    (\r ->
                        case Tuple.second r of
                            TRParen :: rest2 ->
                                Ok ( Tuple.first r, rest2 )

                            TComma :: afterComma ->
                                parseTupleItems afterComma [ Tuple.first r ]

                            _ ->
                                Err "expected a closing )"
                    )

        TLBracket :: rest ->
            parseListItems rest []

        TLBrace :: rest ->
            parseRecord rest

        _ ->
            Err "expected an expression"


{-| Parses the remaining items of a tuple `( e1, e2, … )` (the first item already parsed). -}
parseTupleItems : List Token -> List Expr -> Result String ( Expr, List Token )
parseTupleItems tokens acc =
    parseExpr tokens
        |> Result.andThen
            (\r ->
                case Tuple.second r of
                    TComma :: rest ->
                        parseTupleItems rest (acc ++ [ Tuple.first r ])

                    TRParen :: rest ->
                        Ok ( Tup (acc ++ [ Tuple.first r ]), rest )

                    _ ->
                        Err "expected ',' or ')' in tuple"
            )


{-| A record literal `{ a = e, … }`, an update `{ r | a = e, … }`, or the empty record `{}`. -}
parseRecord : List Token -> Result String ( Expr, List Token )
parseRecord tokens =
    case tokens of
        TRBrace :: rest ->
            Ok ( RecordLit [], rest )

        (TId name) :: TPipe :: afterPipe ->
            parseFields afterPipe []
                |> Result.map (\r -> ( RecordUpdate name (Tuple.first r), Tuple.second r ))

        _ ->
            parseFields tokens []
                |> Result.map (\r -> ( RecordLit (Tuple.first r), Tuple.second r ))


parseFields : List Token -> List ( String, Expr ) -> Result String ( List ( String, Expr ), List Token )
parseFields tokens acc =
    case tokens of
        (TId name) :: TEquals :: afterEq ->
            parseExpr afterEq
                |> Result.andThen
                    (\r ->
                        case Tuple.second r of
                            TComma :: rest2 ->
                                parseFields rest2 (acc ++ [ ( name, Tuple.first r ) ])

                            TRBrace :: rest2 ->
                                Ok ( acc ++ [ ( name, Tuple.first r ) ], rest2 )

                            _ ->
                                Err "expected ',' or '}' in record"
                    )

        _ ->
            Err "expected 'field = value' in record"


parseIf : List Token -> Result String ( Expr, List Token )
parseIf tokens =
    parseExpr tokens
        |> Result.andThen
            (\rc ->
                case Tuple.second rc of
                    (TId "then") :: afterThen ->
                        parseExpr afterThen
                            |> Result.andThen
                                (\rt ->
                                    case Tuple.second rt of
                                        (TId "else") :: afterElse ->
                                            parseExpr afterElse
                                                |> Result.map
                                                    (\re ->
                                                        ( If (Tuple.first rc) (Tuple.first rt) (Tuple.first re)
                                                        , Tuple.second re
                                                        )
                                                    )

                                        _ ->
                                            Err "expected 'else'"
                                )

                    _ ->
                        Err "expected 'then'"
            )


{-| `let` supports several bindings (separated by the `TSemi` that layout inserts), each of which
may take parameters (`let f x = ... in`). Bindings desugar to nested `Let` nodes. -}
parseLet : List Token -> Result String ( Expr, List Token )
parseLet tokens =
    parseLetBinding tokens
        |> Result.andThen
            (\rb ->
                let
                    binding =
                        Tuple.first rb
                in
                case Tuple.second rb of
                    TSemi :: afterSemi ->
                        parseLet afterSemi
                            |> Result.map
                                (\rr -> ( buildLet binding (Tuple.first rr), Tuple.second rr ))

                    (TId "in") :: afterIn ->
                        parseExpr afterIn
                            |> Result.map
                                (\rbody -> ( buildLet binding (Tuple.first rbody), Tuple.second rbody ))

                    _ ->
                        Err "expected 'in'"
            )


{-| Builds a `Let` for one binding around its continuation. A plain binding binds the name directly;
a destructuring binding (`( a, b ) = …` / `{ x } = …`) binds a fresh name to the value and unpacks the
pattern over the continuation (via `wrapDestructures`). -}
buildLet : ( String, Expr, Maybe Pattern ) -> Expr -> Expr
buildLet ( name, value, maybePat ) cont =
    case maybePat of
        Nothing ->
            Let name value cont

        Just pat ->
            Let name value (wrapDestructures [ ( name, pat ) ] cont)


{-| Drops tokens up to and including the next `TSemi` (a let-binding separator) — used to skip a
type-annotation line so the following `name = …` binding is parsed. -}
dropThroughSemi : List Token -> List Token
dropThroughSemi tokens =
    case tokens of
        [] ->
            []

        TSemi :: rest ->
            rest

        _ :: rest ->
            dropThroughSemi rest


parseLetBinding : List Token -> Result String ( ( String, Expr, Maybe Pattern ), List Token )
parseLetBinding tokens =
    case tokens of
        TLParen :: _ ->
            parseLetDestructure tokens

        TLBrace :: _ ->
            parseLetDestructure tokens

        (TId name) :: rest ->
            collectParams rest [] []
                |> Result.andThen
                    (\( params, wrappers, afterParams ) ->
                        case afterParams of
                            TEquals :: afterEq ->
                                parseExpr afterEq
                                    |> Result.map
                                        (\rv ->
                                            let
                                                wrapped =
                                                    wrapDestructures wrappers (Tuple.first rv)

                                                value =
                                                    if List.isEmpty params then
                                                        wrapped

                                                    else
                                                        Lam params wrapped
                                            in
                                            ( ( name, value, Nothing ), Tuple.second rv )
                                        )

                            (TOp ":") :: _ ->
                                -- A type annotation `name : Type`: skip it (up to the next binding
                                -- separator) and parse the actual `name = …` binding that follows.
                                parseLetBinding (dropThroughSemi afterParams)

                            _ ->
                                Err "expected '=' in let binding"
                    )

        _ ->
            Err "expected 'NAME =' after let"


{-| A destructuring let binding `( a, b ) = expr` / `{ x } = expr`: binds the value to a fresh name
and records the pattern so `buildLet` unpacks it over the continuation. -}
parseLetDestructure : List Token -> Result String ( ( String, Expr, Maybe Pattern ), List Token )
parseLetDestructure tokens =
    parsePatternAtom tokens
        |> Result.andThen
            (\( pat, afterPat ) ->
                case afterPat of
                    TEquals :: afterEq ->
                        parseExpr afterEq
                            |> Result.map (\rv -> ( ( "$letd", Tuple.first rv, Just pat ), Tuple.second rv ))

                    _ ->
                        Err "expected '=' in let destructuring binding"
            )


{-| Collects a let/lambda binding's parameters: simple names plus tuple-pattern destructures (each
bound to a fresh `$larg` name and unpacked by `wrapDestructures` in the body). -}
collectParams : List Token -> List String -> List ( String, Pattern ) -> Result String ( List String, List ( String, Pattern ), List Token )
collectParams tokens params wrappers =
    case tokens of
        (TId p) :: rest ->
            collectParams rest (params ++ [ p ]) wrappers

        TLParen :: _ ->
            destructureParam tokens params wrappers

        TLBrace :: _ ->
            -- A record-pattern parameter, e.g. `\{ viewport } -> …` or `f { x, y } = …`.
            destructureParam tokens params wrappers

        _ ->
            Ok ( params, wrappers, tokens )


{-| Collects one destructuring parameter (a tuple `( … )` or record `{ … }` pattern): binds it to a
fresh `$larg` name and records the pattern for `wrapDestructures` to unpack in the body. -}
destructureParam : List Token -> List String -> List ( String, Pattern ) -> Result String ( List String, List ( String, Pattern ), List Token )
destructureParam tokens params wrappers =
    parsePatternAtom tokens
        |> Result.andThen
            (\( pat, after ) ->
                let
                    fresh =
                        "$larg" ++ String.fromInt (List.length params)
                in
                collectParams after (params ++ [ fresh ]) (wrappers ++ [ ( fresh, pat ) ])
            )


parseLambda : List Token -> Result String ( Expr, List Token )
parseLambda tokens =
    collectParams tokens [] []
        |> Result.andThen
            (\( params, wrappers, rest ) ->
                case rest of
                    TArrow :: afterArrow ->
                        if List.isEmpty params then
                            Err "lambda needs a parameter"

                        else
                            parseExpr afterArrow
                                |> Result.map
                                    (\r ->
                                        ( Lam params (wrapDestructures wrappers (Tuple.first r))
                                        , Tuple.second r
                                        )
                                    )

                    _ ->
                        Err "expected lambda parameters then '->'"
            )


parseListItems : List Token -> List Expr -> Result String ( Expr, List Token )
parseListItems tokens acc =
    case tokens of
        TRBracket :: rest ->
            Ok ( ListE acc, rest )

        _ ->
            parseExpr tokens
                |> Result.andThen
                    (\r ->
                        case Tuple.second r of
                            TComma :: rest2 ->
                                parseListItems rest2 (acc ++ [ Tuple.first r ])

                            TRBracket :: rest2 ->
                                Ok ( ListE (acc ++ [ Tuple.first r ]), rest2 )

                            _ ->
                                Err "expected ',' or ']' in list"
                    )


parseCase : List Token -> Result String ( Expr, List Token )
parseCase tokens =
    parseExpr tokens
        |> Result.andThen
            (\rs ->
                case Tuple.second rs of
                    (TId "of") :: afterOf ->
                        parseBranches afterOf []
                            |> Result.map (\rb -> ( Case (Tuple.first rs) (Tuple.first rb), Tuple.second rb ))

                    _ ->
                        Err "expected 'of' after case subject"
            )


parseBranches : List Token -> List ( Pattern, Expr ) -> Result String ( List ( Pattern, Expr ), List Token )
parseBranches tokens acc =
    parsePattern tokens
        |> Result.andThen
            (\rp ->
                case Tuple.second rp of
                    TArrow :: afterArrow ->
                        parseExpr afterArrow
                            |> Result.andThen
                                (\rb ->
                                    let
                                        branches =
                                            acc ++ [ ( Tuple.first rp, Tuple.first rb ) ]
                                    in
                                    case Tuple.second rb of
                                        TSemi :: afterSemi ->
                                            parseBranches afterSemi branches

                                        rest ->
                                            Ok ( branches, rest )
                                )

                    _ ->
                        Err "expected '->' in case branch"
            )


parsePattern : List Token -> Result String ( Pattern, List Token )
parsePattern tokens =
    -- `as` binds loosest, so it wraps a whole (cons) pattern: `x :: xs as all`.
    parseConsPattern tokens
        |> Result.andThen
            (\r ->
                case Tuple.second r of
                    (TId "as") :: (TId name) :: rest ->
                        Ok ( PAlias (Tuple.first r) name, rest )

                    _ ->
                        Ok r
            )


parseConsPattern : List Token -> Result String ( Pattern, List Token )
parseConsPattern tokens =
    parsePatternApp tokens
        |> Result.andThen
            (\r ->
                case Tuple.second r of
                    (TOp "::") :: rest ->
                        parseConsPattern rest
                            |> Result.map (\r2 -> ( PCons (Tuple.first r) (Tuple.first r2), Tuple.second r2 ))

                    _ ->
                        Ok r
            )


parsePatternApp : List Token -> Result String ( Pattern, List Token )
parsePatternApp tokens =
    case tokens of
        (TUpper name) :: rest ->
            parsePatternArgs rest []
                |> Result.map (\r -> ( PCtor name (Tuple.first r), Tuple.second r ))

        _ ->
            parsePatternAtom tokens


parsePatternArgs : List Token -> List Pattern -> Result String ( List Pattern, List Token )
parsePatternArgs tokens acc =
    if startsPatternAtom tokens then
        parsePatternAtom tokens
            |> Result.andThen (\r -> parsePatternArgs (Tuple.second r) (acc ++ [ Tuple.first r ]))

    else
        Ok ( acc, tokens )


startsPatternAtom : List Token -> Bool
startsPatternAtom tokens =
    case tokens of
        (TId _) :: _ ->
            True

        (TUpper _) :: _ ->
            True

        (TNum _) :: _ ->
            True

        (TStr _) :: _ ->
            True

        (TChar _) :: _ ->
            True

        TLParen :: _ ->
            True

        TLBracket :: _ ->
            True

        TLBrace :: _ ->
            True

        _ ->
            False


{-| Parses the remaining items of a tuple pattern `( p1, p2, … )` (first already parsed). -}
parseTuplePat : List Token -> List Pattern -> Result String ( Pattern, List Token )
parseTuplePat tokens acc =
    parsePattern tokens
        |> Result.andThen
            (\r ->
                case Tuple.second r of
                    TComma :: rest ->
                        parseTuplePat rest (acc ++ [ Tuple.first r ])

                    TRParen :: rest ->
                        Ok ( PTup (acc ++ [ Tuple.first r ]), rest )

                    _ ->
                        Err "expected ',' or ')' in tuple pattern"
            )


{-| Parses the field names of a record pattern `{ a, b }` (the opening `{` already consumed). -}
parseRecordPatFields : List Token -> List String -> Result String ( Pattern, List Token )
parseRecordPatFields tokens acc =
    case tokens of
        TRBrace :: rest ->
            Ok ( PRecord acc, rest )

        (TId name) :: TComma :: rest ->
            parseRecordPatFields rest (acc ++ [ name ])

        (TId name) :: TRBrace :: rest ->
            Ok ( PRecord (acc ++ [ name ]), rest )

        _ ->
            Err "expected a field name in record pattern"


parsePatternAtom : List Token -> Result String ( Pattern, List Token )
parsePatternAtom tokens =
    case tokens of
        (TId "_") :: rest ->
            Ok ( PWild, rest )

        (TUpper "True") :: rest ->
            Ok ( PBool True, rest )

        (TUpper "False") :: rest ->
            Ok ( PBool False, rest )

        (TUpper name) :: rest ->
            Ok ( PCtor name [], rest )

        (TId name) :: rest ->
            Ok ( PVar name, rest )

        (TNum n) :: rest ->
            Ok ( PInt n, rest )

        (TOp "-") :: (TNum n) :: rest ->
            -- A negative literal pattern `-1` (a lone `-` then a number, in pattern position).
            Ok ( PInt (negate n), rest )

        (TStr s) :: rest ->
            Ok ( PStr s, rest )

        (TChar ch) :: rest ->
            Ok ( PChar ch, rest )

        TLBracket :: TRBracket :: rest ->
            Ok ( PNil, rest )

        TLParen :: TRParen :: rest ->
            -- The unit pattern `()` (e.g. `init () = …`): unit carries no data, so it always matches.
            Ok ( PWild, rest )

        TLBrace :: rest ->
            -- A record pattern `{ a, b }`: binds the named fields (e.g. `GotViewport { viewport } ->`).
            parseRecordPatFields rest []

        TLParen :: rest ->
            parsePattern rest
                |> Result.andThen
                    (\r ->
                        case Tuple.second r of
                            TRParen :: rest2 ->
                                Ok ( Tuple.first r, rest2 )

                            TComma :: afterComma ->
                                parseTuplePat afterComma [ Tuple.first r ]

                            _ ->
                                Err "expected ')' in pattern"
                    )

        _ ->
            Err "expected a pattern"



-- MODULE PARSER: top-level definitions, split by column-0 lines (layout-lite)


parseProject : List ( String, String ) -> Result String Globals
parseProject files =
    -- foldr so each module's decls (`d`) are *prepended* to the accumulated tail (`d ++ defs`), which
    -- is O(len d) per step rather than the O(len defs) of appending to a growing accumulator — O(n)
    -- overall instead of O(n²). Order and first-error semantics are preserved (Result.map2 keeps the
    -- left/earlier error, and foldr resolves the leftmost module last so its error wins).
    List.foldr
        (\file acc -> Result.map2 (\d defs -> d ++ defs) (parseModule (Tuple.second file)) acc)
        (Ok [])
        files
        |> Result.map (\decls -> Dict.fromList (List.reverse decls))


parseModule : String -> Result String (List ( String, Decl ))
parseModule source =
    -- Collapse multi-line literals (triple strings, glsl shaders) first, so a `"""…"""` whose
    -- continuation lines start at column 0 isn't split across top-level chunks by `startsTopLevel`.
    chunk (String.lines (collapseMultiline source)) [] []
        |> List.filter (\c -> c /= "")
        |> List.foldr
            (\c acc -> Result.map2 (\md defs -> md ++ defs) (parseDecl c) acc)
            (Ok [])


{-| Groups source lines into top-level chunks: a new chunk starts at a non-blank line whose first
character is not whitespace; indented/blank lines continue the current chunk. -}
chunk : List String -> List String -> List String -> List String
chunk lines current done =
    case lines of
        [] ->
            List.reverse (flush current done)

        line :: rest ->
            if startsTopLevel line then
                chunk rest [ line ] (flush current done)

            else
                chunk rest (current ++ [ line ]) done


flush : List String -> List String -> List String
flush current done =
    if List.isEmpty current then
        done

    else
        String.join "\n" current :: done


startsTopLevel : String -> Bool
startsTopLevel line =
    case String.toList line of
        [] ->
            False

        c :: _ ->
            not (c == ' ' || c == '\t')


{-| Parses one top-level chunk into a (possibly empty) list of declarations. Module/import/type
headers and bare type annotations are ignored; a `name params = body` becomes a Decl. -}
parseDecl : String -> Result String (List ( String, Decl ))
parseDecl source =
    let
        trimmed =
            String.trimLeft source

        firstWord =
            trimmed |> String.split " " |> List.head |> Maybe.withDefault ""
    in
    if String.startsWith "type alias " trimmed then
        -- A record type alias doubles as a positional constructor: `Model a b` builds
        -- `{ field1 = a, field2 = b }` in declaration order.
        Ok (typeAliasCtor source)

    else if List.member firstWord [ "module", "import", "type", "port", "" ] || String.startsWith "--" trimmed then
        Ok []

    else
        case tokenize source of
            Err _ ->
                Ok []

            Ok rawTokens ->
                case cookLayout rawTokens of
                    (TId name) :: rest ->
                        parseDeclParams name rest [] []

                    _ ->
                        Ok []


{-| Builds the positional constructor for a record `type alias`. Non-record aliases (no `{ ... }`)
register nothing. Field names are read directly from the source (the `:` in field annotations is
not a lexable token), so this stays independent of the expression tokenizer. -}
typeAliasCtor : String -> List ( String, Decl )
typeAliasCtor source =
    case ( aliasName source, recordFields source ) of
        ( Just nm, fields ) ->
            if List.isEmpty fields then
                []

            else
                [ ( nm, { name = nm, params = fields, body = RecordLit (List.map (\f -> ( f, Var f )) fields) } ) ]

        _ ->
            []


aliasName : String -> Maybe String
aliasName source =
    case String.words source of
        "type" :: "alias" :: nm :: _ ->
            Just nm

        _ ->
            Nothing


recordFields : String -> List String
recordFields source =
    case String.split "{" source of
        _ :: afterBrace :: _ ->
            case String.split "}" afterBrace of
                inner :: _ ->
                    inner
                        |> String.split ","
                        |> List.map (\part -> part |> String.split ":" |> List.head |> Maybe.withDefault "" |> String.trim)
                        |> List.filter (\f -> f /= "")

                _ ->
                    []

        _ ->
            []


parseDeclParams : String -> List Token -> List String -> List ( String, Pattern ) -> Result String (List ( String, Decl ))
parseDeclParams name tokens params wrappers =
    case tokens of
        (TId p) :: rest ->
            parseDeclParams name rest (params ++ [ p ]) wrappers

        TLParen :: _ ->
            -- A destructuring parameter (a tuple pattern like `(x, y)`): bind a fresh name and
            -- `case` on it in the body, so `f (x, y) = …` works without changing the value model.
            declDestructure name tokens params wrappers

        TLBrace :: _ ->
            -- A record-pattern parameter, e.g. `f { x, y } = …`.
            declDestructure name tokens params wrappers

        TEquals :: rest ->
            parse rest
                |> Result.map
                    (\body ->
                        [ ( name, { name = name, params = params, body = wrapDestructures wrappers body } ) ]
                    )

        _ ->
            -- not a value/function definition (e.g. an annotation `name : Type`): ignore
            Ok []


{-| Collects one destructuring top-level parameter (tuple or record pattern), binding it to a fresh
`$arg` name unpacked by `wrapDestructures` in the body. -}
declDestructure : String -> List Token -> List String -> List ( String, Pattern ) -> Result String (List ( String, Decl ))
declDestructure name tokens params wrappers =
    case parsePatternAtom tokens of
        Ok ( pat, after ) ->
            let
                fresh =
                    "$arg" ++ String.fromInt (List.length params)
            in
            parseDeclParams name after (params ++ [ fresh ]) (wrappers ++ [ ( fresh, pat ) ])

        Err e ->
            Err e


{-| Wraps a body in a `case` per destructuring parameter, binding its pattern against the fresh name. -}
wrapDestructures : List ( String, Pattern ) -> Expr -> Expr
wrapDestructures wrappers body =
    List.foldr (\( fresh, pat ) acc -> Case (Var fresh) [ ( pat, acc ) ]) body wrappers
