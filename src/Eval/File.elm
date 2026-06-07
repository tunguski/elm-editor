module Eval.File exposing (processor)

{-| The interpreter's `File.*` builtins, as an {@link Eval.Core.Processor}. A file is `VCtor "File"
[ name, content ]`; `File.Select.*` are commands the editor runs with a real picker, and
`File.toString`/`toUrl` are tasks resolved immediately (the content is already in hand). -}

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
    [ "File.Select.file", "File.Select.files", "File.name", "File.mime", "File.size", "File.toString", "File.toUrl" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "File.name", "File.mime", "File.size", "File.toString", "File.toUrl" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "File.Select.file", [ _, toMsg ] ) ->
            Just (Ok (VCtor "Cmd.fileSelect" [ toMsg ]))

        ( "File.Select.files", [ _, toMsg ] ) ->
            -- `files` takes `File -> List File -> msg`; flag it so the picked file is delivered as
            -- (file, []) rather than only the first argument (which left an unsaturated message).
            Just (Ok (VCtor "Cmd.fileSelectMany" [ toMsg ]))

        ( "File.name", [ VCtor "File" [ name_, _ ] ] ) ->
            Just (Ok name_)

        ( "File.mime", [ VCtor "File" _ ] ) ->
            Just (Ok (VStr "text/plain"))

        ( "File.size", [ VCtor "File" [ _, VStr content ] ] ) ->
            Just (Ok (VNum (toFloat (String.length content))))

        ( "File.toString", [ VCtor "File" [ _, content ] ] ) ->
            Just (Ok (VCtor "Task.value" [ content ]))

        ( "File.toUrl", [ VCtor "File" [ _, content ] ] ) ->
            Just (Ok (VCtor "Task.value" [ content ]))

        _ ->
            Nothing
