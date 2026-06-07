module Eval.Events exposing (processor)

{-| The interpreter's `Browser.Events` subscription builtins, as an {@link Eval.Core.Processor}. The
editor drives `onAnimationFrameDelta`/key/resize/mouse live (each becomes a tagged `Sub.*` the editor
inspects); the rest are accepted as opaque no-op subscriptions so the program still runs. -}

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
    [ "onAnimationFrameDelta", "onAnimationFrame", "onResize", "onMouseMove", "onMouseDown", "onMouseUp", "onKeyDown", "onKeyUp", "onKeyPress", "onVisibilityChange" ]


arities : List ( Int, List String )
arities =
    [ ( 1, names ) ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    if List.member name names then
        Just (Ok (sub name args))

    else
        Nothing


sub : String -> List Value -> Value
sub name args =
    case name of
        "onAnimationFrameDelta" ->
            -- The editor feeds a frame's delta (ms) to the toMsg each animation frame.
            case args of
                [ toMsg ] ->
                    VCtor "Sub.animationFrame" [ toMsg ]

                _ ->
                    VCtor "Sub" []

        "onKeyDown" ->
            VCtor "Sub.keyDown" args

        "onKeyUp" ->
            VCtor "Sub.keyUp" args

        "onResize" ->
            VCtor "Sub.resize" args

        "onMouseMove" ->
            VCtor "Sub.mouseMove" args

        _ ->
            VCtor "Sub" []
