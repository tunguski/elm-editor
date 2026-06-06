module Share exposing (encodeFiles, decodeFiles)

{-| Encode/decode an editor session's files to and from a single self-describing string, so a session
can be shared (e.g. put in a URL fragment, or pasted into the editor to restore it). Pure and
reversible: `decodeFiles (encodeFiles files) == files`. The browser glue (writing the string to the
location hash / clipboard, reading it back at startup) lives outside this module.

Format: each file is a length-prefixed name then a length-prefixed content, concatenated —
`<nameLen>,<name><contentLen>,<content>` — so any characters (including separators and newlines) in
names or content round-trip without escaping.
-}


{-| Encodes a list of `(name, content)` files to one string. -}
encodeFiles : List ( String, String ) -> String
encodeFiles files =
    String.concat (List.map encodeOne files)


encodeOne : ( String, String ) -> String
encodeOne ( name, content ) =
    field name ++ field content


field : String -> String
field s =
    String.fromInt (String.length s) ++ "," ++ s


{-| Decodes a string produced by `encodeFiles` back to the list of files. Malformed input yields the
files decoded so far (best effort). -}
decodeFiles : String -> List ( String, String )
decodeFiles s =
    decodeHelp s []


decodeHelp : String -> List ( String, String ) -> List ( String, String )
decodeHelp s acc =
    if s == "" then
        List.reverse acc

    else
        case readField s of
            Just ( name, afterName ) ->
                case readField afterName of
                    Just ( content, rest ) ->
                        decodeHelp rest (( name, content ) :: acc)

                    Nothing ->
                        List.reverse acc

            Nothing ->
                List.reverse acc


{-| Reads one `<len>,<text>` field, returning the text and the remaining string. -}
readField : String -> Maybe ( String, String )
readField s =
    case String.indexes "," s of
        comma :: _ ->
            case String.toInt (String.left comma s) of
                Just len ->
                    let
                        body =
                            String.dropLeft (comma + 1) s
                    in
                    Just ( String.left len body, String.dropLeft len body )

                Nothing ->
                    Nothing

        [] ->
            Nothing
