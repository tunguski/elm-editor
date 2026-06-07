module Eval.Http exposing (processor)

{-| The interpreter's `Http.*` builtins, as an {@link Eval.Core.Processor}. `Http.get` becomes a
command the editor issues for real, feeding the response back through the `expect`. -}

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
    [ "Http.expectString", "Http.expectJson", "Http.get" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "Http.expectString", "Http.get" ] ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "Http.expectString", [ toMsg ] ) ->
            Just (Ok (VCtor "Http.expect" [ toMsg ]))

        ( "Http.expectJson", [ toMsg, decoder ] ) ->
            Just (Ok (VCtor "Http.expectJson" [ toMsg, decoder ]))

        ( "Http.get", [ VRecord fields ] ) ->
            Just
                (case ( field "url" fields, field "expect" fields ) of
                    ( Just (VStr url), Just expect ) ->
                        Ok (VCtor "Cmd.http" [ VStr url, expect ])

                    _ ->
                        Err "Http.get needs { url : String, expect : … }"
                )

        _ ->
            Nothing


field : String -> List ( String, Value ) -> Maybe Value
field key fields =
    case List.filter (\( k, _ ) -> k == key) fields of
        ( _, v ) :: _ ->
            Just v

        [] ->
            Nothing
