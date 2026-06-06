module Assist exposing (completions, wordAt, accept, errorName, squiggleFor, offsetOf)

{-| Editor assistance as pure functions the browser UI calls — no DOM, no effects, so it is fully
testable. Two features:

  - **Autocomplete**: `wordAt` extracts the (possibly qualified) identifier the caret sits in, and
    `completions` offers matching candidates drawn from keywords, common built-ins and the
    identifiers already present in the buffer.
  - **Inline error squiggles**: `squiggleFor` finds where an offending identifier first appears, as a
    0-based `{ line, column, length }` the UI underlines.

-}


keywords : List String
keywords =
    [ "if", "then", "else", "let", "in", "case", "of", "type", "alias"
    , "module", "import", "exposing", "port", "as"
    ]


{-| A curated set of commonly-used (qualified) built-ins, offered alongside in-buffer identifiers. -}
builtins : List String
builtins =
    [ "List.map", "List.filter", "List.foldl", "List.foldr", "List.range", "List.length"
    , "List.reverse", "List.member", "List.concat", "List.indexedMap", "List.append"
    , "String.length", "String.toUpper", "String.toLower", "String.fromInt", "String.toInt"
    , "String.split", "String.join", "String.contains", "String.left", "String.dropLeft"
    , "Maybe.withDefault", "Maybe.map", "Result.map", "Just", "Nothing", "Ok", "Err"
    , "abs", "modBy", "toFloat", "round", "floor", "ceiling", "truncate", "sqrt"
    ]


{-| Completion candidates for `prefix`: keywords, common built-ins and the identifiers already in
`source`, filtered to those starting with `prefix`, sorted and de-duplicated. The prefix itself is
never offered, and an empty prefix yields nothing (so the UI doesn't pop up on every keystroke). -}
completions : String -> String -> List String
completions source prefix =
    if prefix == "" then
        []

    else
        (keywords ++ builtins ++ identifiers source)
            |> List.filter (\c -> c /= prefix && String.startsWith prefix c)
            |> dedupeSort


{-| The identifier (possibly qualified, e.g. `List.ma`) ending at character `offset` — the word the
caret is in the middle of, which the UI feeds to `completions`. -}
wordAt : String -> Int -> String
wordAt source offset =
    String.left offset source
        |> String.toList
        |> List.reverse
        |> spanWhile isIdentChar
        |> Tuple.first
        |> List.reverse
        |> String.fromList


{-| Accepts a completion: replaces the word ending at character `offset` in `source` with
`completion`, returning the new source and the new caret offset (just past the inserted text). -}
accept : String -> Int -> String -> ( String, Int )
accept source offset completion =
    let
        prefix =
            wordAt source offset

        start =
            offset - String.length prefix
    in
    ( String.left start source ++ completion ++ String.dropLeft offset source
    , start + String.length completion
    )


{-| The offending identifier named in an error message, for locating a squiggle. Errors read like
"undefined variable: nope" or "Naming error: foo is not in scope" — the name follows the last colon.
`Nothing` if the message names nothing. -}
errorName : String -> Maybe String
errorName message =
    case List.reverse (String.split ":" message) of
        tail :: _ :: _ ->
            let
                ( word, _ ) =
                    spanWhile isIdentChar (String.toList (String.trimLeft tail))

                w =
                    String.fromList word
            in
            if w == "" then
                Nothing

            else
                Just w

        _ ->
            Nothing


{-| Where the identifier `name` first appears in `source` as a whole word, as a 0-based
`{ line, column, length }` for drawing an inline squiggle under an error like
"undefined variable: name". `Nothing` if it never occurs (or `name` is empty). -}
squiggleFor : String -> String -> Maybe { line : Int, column : Int, length : Int }
squiggleFor source name =
    if name == "" then
        Nothing

    else
        findLine 0 (String.lines source) name


{-| The character offset into `source` of the 0-based `line`/`column` produced by `squiggleFor` —
the start index for slicing out the squiggled range (assumes `\n` line endings, like `squiggleFor`). -}
offsetOf : Int -> Int -> String -> Int
offsetOf line column source =
    let
        before =
            List.take line (String.lines source)
    in
    List.sum (List.map String.length before) + line + column


findLine : Int -> List String -> String -> Maybe { line : Int, column : Int, length : Int }
findLine i lines name =
    case lines of
        [] ->
            Nothing

        line :: rest ->
            case indexOfWord line name 0 of
                Just col ->
                    Just { line = i, column = col, length = String.length name }

                Nothing ->
                    findLine (i + 1) rest name



-- helpers -----------------------------------------------------------------


identifiers : String -> List String
identifiers source =
    collectIds (String.toList source) []


collectIds : List Char -> List String -> List String
collectIds chars acc =
    case chars of
        [] ->
            List.reverse acc

        c :: rest ->
            if isIdentStart c then
                let
                    ( word, after ) =
                        spanWhile isIdentChar chars
                in
                collectIds after (String.fromList word :: acc)

            else
                collectIds rest acc


isIdentStart : Char -> Bool
isIdentStart c =
    let
        n =
            Char.toCode c
    in
    (n >= 65 && n <= 90) || (n >= 97 && n <= 122) || c == '_'


isIdentChar : Char -> Bool
isIdentChar c =
    let
        n =
            Char.toCode c
    in
    (n >= 65 && n <= 90) || (n >= 97 && n <= 122) || (n >= 48 && n <= 57) || c == '_' || c == '.'


{-| Splits off the longest prefix of `chars` all satisfying `pred`. -}
spanWhile : (Char -> Bool) -> List Char -> ( List Char, List Char )
spanWhile pred chars =
    case chars of
        c :: rest ->
            if pred c then
                let
                    ( taken, left ) =
                        spanWhile pred rest
                in
                ( c :: taken, left )

            else
                ( [], chars )

        [] ->
            ( [], [] )


dedupeSort : List String -> List String
dedupeSort xs =
    dedupeSorted (List.sort xs)


dedupeSorted : List String -> List String
dedupeSorted xs =
    case xs of
        a :: b :: rest ->
            if a == b then
                dedupeSorted (b :: rest)

            else
                a :: dedupeSorted (b :: rest)

        _ ->
            xs


{-| The index of the first whole-word occurrence of `name` in `line` at or after `from`, or Nothing.
A "whole word" has no identifier character immediately before or after it. -}
indexOfWord : String -> String -> Int -> Maybe Int
indexOfWord line name from =
    let
        n =
            String.length name

        len =
            String.length line
    in
    if from + n > len then
        Nothing

    else if
        String.slice from (from + n) line
            == name
            && boundaryOk (charAt (from - 1) line) (charAt (from + n) line)
    then
        Just from

    else
        indexOfWord line name (from + 1)


charAt : Int -> String -> Maybe Char
charAt i s =
    if i < 0 then
        Nothing

    else
        List.head (String.toList (String.slice i (i + 1) s))


boundaryOk : Maybe Char -> Maybe Char -> Bool
boundaryOk left right =
    notIdent left && notIdent right


notIdent : Maybe Char -> Bool
notIdent mc =
    case mc of
        Just c ->
            not (isIdentChar c)

        Nothing ->
            True
