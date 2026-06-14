module Eval.Url exposing (processor)

{-| The interpreter's `Url.*` builtins, as an {@link Eval.Core.Processor}. A `Url` is the
elm/url-shaped record `{ protocol, host, port_, path, query, fragment }` (protocol is the ctor
`Http`/`Https`; the optional fields are `Just`/`Nothing`). `fromString` follows elm/url's own parse
algorithm; `percentEncode`/`percentDecode` are RFC 3986 over UTF-8. -}

import Eval.Core exposing (Core, Processor)
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


names : List String
names =
    [ "Url.fromString", "Url.toString", "Url.percentEncode", "Url.percentDecode" ]


arities : List ( Int, List String )
arities =
    [ ( 1, names ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "Url.fromString", [ VStr s ] ) ->
            Just (Ok (maybeValue (fromString s)))

        ( "Url.toString", [ url ] ) ->
            Just (Ok (VStr (toString url)))

        ( "Url.percentEncode", [ VStr s ] ) ->
            Just (Ok (VStr (percentEncode s)))

        ( "Url.percentDecode", [ VStr s ] ) ->
            Just (Ok (maybeValue (Maybe.map VStr (percentDecode s))))

        _ ->
            Nothing


maybeValue : Maybe Value -> Value
maybeValue m =
    case m of
        Just v ->
            VCtor "Just" [ v ]

        Nothing ->
            VCtor "Nothing" []



-- fromString (the elm/url algorithm) -------------------------------------


fromString : String -> Maybe Value
fromString str =
    if String.startsWith "http://" str then
        chompAfterProtocol "Http" (String.dropLeft 7 str)

    else if String.startsWith "https://" str then
        chompAfterProtocol "Https" (String.dropLeft 8 str)

    else
        Nothing


chompAfterProtocol : String -> String -> Maybe Value
chompAfterProtocol protocol str =
    if String.isEmpty str then
        Nothing

    else
        case String.indexes "#" str of
            [] ->
                chompBeforeFragment protocol Nothing str

            i :: _ ->
                chompBeforeFragment protocol (Just (String.dropLeft (i + 1) str)) (String.left i str)


chompBeforeFragment : String -> Maybe String -> String -> Maybe Value
chompBeforeFragment protocol frag str =
    if String.isEmpty str then
        Nothing

    else
        case String.indexes "?" str of
            [] ->
                chompBeforeQuery protocol Nothing frag str

            i :: _ ->
                chompBeforeQuery protocol (Just (String.dropLeft (i + 1) str)) frag (String.left i str)


chompBeforeQuery : String -> Maybe String -> Maybe String -> String -> Maybe Value
chompBeforeQuery protocol params frag str =
    if String.isEmpty str then
        Nothing

    else
        case String.indexes "/" str of
            [] ->
                chompAfterAuthority protocol params frag str ""

            i :: _ ->
                chompAfterAuthority protocol params frag (String.left i str) (String.dropLeft i str)


chompAfterAuthority : String -> Maybe String -> Maybe String -> String -> String -> Maybe Value
chompAfterAuthority protocol params frag authority path =
    if String.isEmpty authority then
        Nothing

    else
        case String.indexes ":" authority of
            [] ->
                Just (urlRecord protocol authority Nothing path params frag)

            i :: _ ->
                case String.toInt (String.dropLeft (i + 1) authority) of
                    Nothing ->
                        Nothing

                    Just thePort ->
                        Just (urlRecord protocol (String.left i authority) (Just thePort) path params frag)


urlRecord : String -> String -> Maybe Int -> String -> Maybe String -> Maybe String -> Value
urlRecord protocol host thePort path params frag =
    VRecord
        [ ( "protocol", VCtor protocol [] )
        , ( "host", VStr host )
        , ( "port_", maybeNum thePort )
        , ( "path", VStr path )
        , ( "query", maybeStr params )
        , ( "fragment", maybeStr frag )
        ]


maybeNum : Maybe Int -> Value
maybeNum m =
    case m of
        Just n ->
            VCtor "Just" [ VNum (toFloat n) ]

        Nothing ->
            VCtor "Nothing" []


maybeStr : Maybe String -> Value
maybeStr m =
    case m of
        Just s ->
            VCtor "Just" [ VStr s ]

        Nothing ->
            VCtor "Nothing" []



-- toString ----------------------------------------------------------------


toString : Value -> String
toString url =
    case url of
        VRecord fields ->
            let
                get k =
                    fields |> List.filter (\( n, _ ) -> n == k) |> List.head |> Maybe.map Tuple.second

                scheme =
                    case get "protocol" of
                        Just (VCtor "Https" _) ->
                            "https://"

                        _ ->
                            "http://"

                addPort acc =
                    case get "port_" of
                        Just (VCtor "Just" [ VNum n ]) ->
                            acc ++ ":" ++ String.fromInt (round n)

                        _ ->
                            acc

                str k =
                    case get k of
                        Just (VStr s) ->
                            s

                        _ ->
                            ""

                addOpt prefix k acc =
                    case get k of
                        Just (VCtor "Just" [ VStr s ]) ->
                            acc ++ prefix ++ s

                        _ ->
                            acc
            in
            (scheme ++ str "host")
                |> addPort
                |> (\acc -> acc ++ str "path")
                |> addOpt "?" "query"
                |> addOpt "#" "fragment"

        _ ->
            ""



-- percentEncode / percentDecode (RFC 3986 over UTF-8) ---------------------


percentEncode : String -> String
percentEncode s =
    String.foldr (\c acc -> encodeChar c ++ acc) "" s


encodeChar : Char -> String
encodeChar c =
    let
        code =
            Char.toCode c
    in
    if isUnreserved code then
        String.fromChar c

    else
        utf8Bytes code |> List.map byteToHex |> String.concat


isUnreserved : Int -> Bool
isUnreserved code =
    (code >= 0x41 && code <= 0x5A)
        || (code >= 0x61 && code <= 0x7A)
        || (code >= 0x30 && code <= 0x39)
        || code == 0x2D
        || code == 0x5F
        || code == 0x2E
        || code == 0x7E


utf8Bytes : Int -> List Int
utf8Bytes code =
    if code < 0x80 then
        [ code ]

    else if code < 0x800 then
        [ 0xC0 + (code // 64), 0x80 + modBy 64 code ]

    else if code < 0x10000 then
        [ 0xE0 + (code // 4096), 0x80 + modBy 64 (code // 64), 0x80 + modBy 64 code ]

    else
        [ 0xF0 + (code // 262144)
        , 0x80 + modBy 64 (code // 4096)
        , 0x80 + modBy 64 (code // 64)
        , 0x80 + modBy 64 code
        ]


byteToHex : Int -> String
byteToHex b =
    "%" ++ String.fromChar (hexDigit (b // 16)) ++ String.fromChar (hexDigit (modBy 16 b))


hexDigit : Int -> Char
hexDigit n =
    if n < 10 then
        Char.fromCode (0x30 + n)

    else
        Char.fromCode (0x41 + n - 10)


percentDecode : String -> Maybe String
percentDecode s =
    decodeBytes (String.toList s) []


{-| Walks the input, turning `%XX` escapes (and raw chars) into UTF-8 bytes, then decodes the byte
run back to a String. Returns Nothing on a malformed escape. -}
decodeBytes : List Char -> List Int -> Maybe String
decodeBytes chars acc =
    case chars of
        [] ->
            Just (utf8Decode (List.reverse acc))

        '%' :: hi :: lo :: rest ->
            case ( hexValue hi, hexValue lo ) of
                ( Just h, Just l ) ->
                    decodeBytes rest (h * 16 + l :: acc)

                _ ->
                    Nothing

        '%' :: _ ->
            Nothing

        c :: rest ->
            decodeBytes rest (List.reverse (utf8Bytes (Char.toCode c)) ++ acc)


hexValue : Char -> Maybe Int
hexValue c =
    let
        code =
            Char.toCode c
    in
    if code >= 0x30 && code <= 0x39 then
        Just (code - 0x30)

    else if code >= 0x41 && code <= 0x46 then
        Just (code - 0x41 + 10)

    else if code >= 0x61 && code <= 0x66 then
        Just (code - 0x61 + 10)

    else
        Nothing


{-| Decodes a list of UTF-8 bytes to a String (malformed runs degrade to the replacement char). -}
utf8Decode : List Int -> String
utf8Decode bytes =
    case bytes of
        [] ->
            ""

        b0 :: rest ->
            if b0 < 0x80 then
                String.fromChar (Char.fromCode b0) ++ utf8Decode rest

            else if b0 < 0xE0 then
                cont 1 (b0 - 0xC0) rest

            else if b0 < 0xF0 then
                cont 2 (b0 - 0xE0) rest

            else
                cont 3 (b0 - 0xF0) rest


cont : Int -> Int -> List Int -> String
cont n code bytes =
    if n == 0 then
        String.fromChar (Char.fromCode code) ++ utf8Decode bytes

    else
        case bytes of
            b :: rest ->
                cont (n - 1) (code * 64 + modBy 64 b) rest

            [] ->
                String.fromChar (Char.fromCode 0xFFFD)
