module Eval.Char exposing (processor)

{-| The interpreter's `Char.*` builtins, as an {@link Eval.Core.Processor}. All pure (no `Core`). -}

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
    [ "Char.toCode", "Char.fromCode", "Char.toUpper", "Char.toLower", "Char.toLocaleUpper", "Char.toLocaleLower", "Char.isDigit", "Char.isUpper", "Char.isLower", "Char.isAlpha", "Char.isAlphaNum", "Char.isSpace", "Char.isHexDigit", "Char.isOctDigit", "Char.isControl", "Char.isPunctuation" ]


arities : List ( Int, List String )
arities =
    [ ( 1, names ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "Char.toCode", [ VChar c ] ) ->
            Just (Ok (VNum (toFloat (Char.toCode c))))

        ( "Char.fromCode", [ VNum n ] ) ->
            Just (Ok (VChar (Char.fromCode (round n))))

        ( "Char.toUpper", [ VChar c ] ) ->
            Just (Ok (VChar (Char.toUpper c)))

        ( "Char.toLower", [ VChar c ] ) ->
            Just (Ok (VChar (Char.toLower c)))

        ( "Char.toLocaleUpper", [ VChar c ] ) ->
            Just (Ok (VChar (Char.toUpper c)))

        ( "Char.toLocaleLower", [ VChar c ] ) ->
            Just (Ok (VChar (Char.toLower c)))

        ( "Char.isDigit", [ VChar c ] ) ->
            Just (Ok (VBool (Char.isDigit c)))

        ( "Char.isUpper", [ VChar c ] ) ->
            Just (Ok (VBool (Char.isUpper c)))

        ( "Char.isLower", [ VChar c ] ) ->
            Just (Ok (VBool (Char.isLower c)))

        ( "Char.isAlpha", [ VChar c ] ) ->
            Just (Ok (VBool (Char.isAlpha c)))

        ( "Char.isAlphaNum", [ VChar c ] ) ->
            Just (Ok (VBool (Char.isAlphaNum c)))

        ( "Char.isSpace", [ VChar c ] ) ->
            Just (Ok (VBool (c == ' ' || c == '\n' || c == '\t' || c == '\u{000D}')))

        ( "Char.isHexDigit", [ VChar c ] ) ->
            Just (Ok (VBool (Char.isDigit c || (Char.toLower c >= 'a' && Char.toLower c <= 'f'))))

        ( "Char.isOctDigit", [ VChar c ] ) ->
            Just (Ok (VBool (c >= '0' && c <= '7')))

        ( "Char.isControl", [ VChar c ] ) ->
            Just (Ok (VBool (Char.isControl c)))

        ( "Char.isPunctuation", [ VChar c ] ) ->
            Just (Ok (VBool (Char.isPunctuation c)))

        _ ->
            Nothing
