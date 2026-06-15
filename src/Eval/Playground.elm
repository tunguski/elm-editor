module Eval.Playground exposing
    ( processor
    , gameInitMem, gameStep, gameView
    , playgroundColor
    )

{-| The evancz/elm-playground subset of the editor's interpreter: shape construction, SVG rendering,
and the game/animation loop. A closed world (shapes in, SVG out) that needs the evaluator only to
apply a game's `view`/`update` and to resolve `main`, so those (`applyValue`, `mainValue`) are passed
in as parameters rather than imported — keeping this a leaf with no import cycle back into `Eval`.
`Eval` re-exposes `gameInitMem`/`gameView`/`gameStep` (wrapping them with its own evaluator). -}

import Eval.Core exposing (Core, Processor)
import Lang exposing (Globals, Value(..))
import Parser exposing (parseProject)


{-| The playground builtins as a {@link Eval.Core.Processor}, folded into `Eval`'s dispatch like every
other builtin module. (The game-loop entry points `gameInitMem`/`gameStep`/`gameView` are not
builtins — the editor calls them directly — so they stay separate.) -}
processor : Processor
processor =
    { names = playgroundNames
    , arities = playgroundArities
    , run = run
    }


{-| The unqualified playground builtins (shapes, transforms, `picture`/`animation`/`game`). `circle`
is ambiguous with SVG `circle`; it is listed so name-based dispatch routes it here first, and `run`
disambiguates it by its arguments — an SVG `circle attrs children` makes `run` decline so dispatch
falls through to `Eval.Render`. (Both spellings are arity 2, so the shared default arity is correct.) -}
playgroundNames : List String
playgroundNames =
    [ "circle", "picture", "animation", "game", "oval", "rectangle", "square", "triangle", "pentagon", "hexagon", "octagon", "words", "image" ]
        ++ [ "move", "moveUp", "moveDown", "moveLeft", "moveRight", "moveX", "moveY", "rotate", "scale", "fade" ]
        ++ [ "rgb", "spin", "wave", "zigzag", "toX", "toY", "degrees" ]


playgroundArities : List ( Int, List String )
playgroundArities =
    [ ( 1, [ "picture", "animation", "toX", "toY", "degrees" ] )
    , ( 3, [ "oval", "rectangle", "move", "rgb", "game", "image" ] )
    , ( 4, [ "wave", "zigzag" ] )
    ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ globals name args =
    if name == "circle" then
        -- `circle color radius` is a playground shape; an SVG `circle attrs children` is NOT one, so
        -- decline (Nothing) and let dispatch fall through to Eval.Render. (circle is in playgroundNames
        -- only so name-based dispatch routes it here first — it must not reach the runPlayground catch
        -- below, which would wrongly report "bad arguments to Playground.circle".)
        if playgroundCircle args then
            Just (Ok (mkShape (VCtor "PCircle" args)))

        else
            Nothing

    else if List.member name playgroundNames then
        Just (runPlayground globals name args)

    else
        Nothing


{-| Assoc-list lookup (a local copy so this module needn't import Eval). -}
lookup : String -> List ( String, a ) -> Maybe a
lookup name pairs =
    case pairs of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if k == name then
                Just v

            else
                lookup name rest


{-| The evaluator entry points the game loop needs, injected by `Eval` to avoid an import cycle:
`Resolve` evaluates a project's `main` (Eval.mainValue), `ApplyTo` applies a value (Eval.applyValue). -}
type alias Resolve =
    List ( String, String ) -> Result String Value


type alias ApplyTo =
    Globals -> Value -> Value -> Result String Value


playgroundCircle : List Value -> Bool
playgroundCircle args =
    case args of
        [ VStr _, VNum _ ] ->
            True

        _ ->
            False


{-| A fresh shape at the origin: PShape form x y angle scale alpha. -}
mkShape : Value -> Value
mkShape form =
    VCtor "PShape" [ form, VNum 0, VNum 0, VNum 0, VNum 1, VNum 1 ]


{-| Rebuilds a shape from its updated transform fields. -}
withShape : Value -> (Value -> Float -> Float -> Float -> Float -> Float -> Value) -> Result String Value
withShape shape f =
    case shape of
        VCtor "PShape" [ form, VNum x, VNum y, VNum a, VNum sc, VNum al ] ->
            Ok (f form x y a sc al)

        _ ->
            Err "expected a shape"


runPlayground : Globals -> String -> List Value -> Result String Value
runPlayground globals name args =
    case ( name, args ) of
        ( "picture", [ VList shapes ] ) ->
            Ok (pictureSvg shapes)

        ( "animation", [ view ] ) ->
            -- Preserve the view so the editor can drive an animation-frame loop (advancing `time`);
            -- a static render (tests / renderProgram) draws the initial frame at time 0.
            Ok (VCtor "Playground.animation" [ view ])

        ( "game", [ view, update, mem ] ) ->
            -- Preserve the parts so the editor can drive the game (keyboard/frames); a static
            -- render (tests / renderProgram) draws the initial frame via gameInitialView.
            Ok (VCtor "Playground.game" [ view, update, mem ])

        ( "image", [ VNum w, VNum h, VStr url ] ) ->
            Ok (mkShape (VCtor "PImage" [ VNum w, VNum h, VStr url ]))

        ( "degrees", [ VNum d ] ) ->
            Ok (VNum (d * pi / 180))

        ( "toX", [ kb ] ) ->
            Ok (VNum (boolField "right" kb - boolField "left" kb))

        ( "toY", [ kb ] ) ->
            Ok (VNum (boolField "up" kb - boolField "down" kb))

        ( "rectangle", [ color, VNum w, VNum h ] ) ->
            Ok (mkShape (VCtor "PRect" [ color, VNum w, VNum h ]))

        ( "square", [ color, VNum s ] ) ->
            Ok (mkShape (VCtor "PRect" [ color, VNum s, VNum s ]))

        ( "oval", [ color, VNum w, VNum h ] ) ->
            Ok (mkShape (VCtor "POval" [ color, VNum w, VNum h ]))

        ( "triangle", [ color, VNum r ] ) ->
            Ok (mkShape (VCtor "PNgon" [ color, VNum 3, VNum r ]))

        ( "pentagon", [ color, VNum r ] ) ->
            Ok (mkShape (VCtor "PNgon" [ color, VNum 5, VNum r ]))

        ( "hexagon", [ color, VNum r ] ) ->
            Ok (mkShape (VCtor "PNgon" [ color, VNum 6, VNum r ]))

        ( "octagon", [ color, VNum r ] ) ->
            Ok (mkShape (VCtor "PNgon" [ color, VNum 8, VNum r ]))

        ( "words", [ color, VStr s ] ) ->
            Ok (mkShape (VCtor "PWords" [ color, VStr s ]))

        ( "move", [ VNum dx, VNum dy, shape ] ) ->
            withShape shape (\f x y a sc al -> VCtor "PShape" [ f, VNum (x + dx), VNum (y + dy), VNum a, VNum sc, VNum al ])

        ( "moveUp", [ VNum d, shape ] ) ->
            withShape shape (\f x y a sc al -> VCtor "PShape" [ f, VNum x, VNum (y + d), VNum a, VNum sc, VNum al ])

        ( "moveDown", [ VNum d, shape ] ) ->
            withShape shape (\f x y a sc al -> VCtor "PShape" [ f, VNum x, VNum (y - d), VNum a, VNum sc, VNum al ])

        ( "moveLeft", [ VNum d, shape ] ) ->
            withShape shape (\f x y a sc al -> VCtor "PShape" [ f, VNum (x - d), VNum y, VNum a, VNum sc, VNum al ])

        ( "moveRight", [ VNum d, shape ] ) ->
            withShape shape (\f x y a sc al -> VCtor "PShape" [ f, VNum (x + d), VNum y, VNum a, VNum sc, VNum al ])

        ( "moveX", [ VNum d, shape ] ) ->
            withShape shape (\f x y a sc al -> VCtor "PShape" [ f, VNum (x + d), VNum y, VNum a, VNum sc, VNum al ])

        ( "moveY", [ VNum d, shape ] ) ->
            withShape shape (\f x y a sc al -> VCtor "PShape" [ f, VNum x, VNum (y + d), VNum a, VNum sc, VNum al ])

        ( "rotate", [ VNum da, shape ] ) ->
            withShape shape (\f x y a sc al -> VCtor "PShape" [ f, VNum x, VNum y, VNum (a + da), VNum sc, VNum al ])

        ( "scale", [ VNum k, shape ] ) ->
            withShape shape (\f x y a sc al -> VCtor "PShape" [ f, VNum x, VNum y, VNum a, VNum (sc * k), VNum al ])

        ( "fade", [ VNum o, shape ] ) ->
            withShape shape (\f x y a sc al -> VCtor "PShape" [ f, VNum x, VNum y, VNum a, VNum sc, VNum o ])

        ( "rgb", [ VNum r, VNum g, VNum b ] ) ->
            Ok (VStr ("rgb(" ++ ic r ++ "," ++ ic g ++ "," ++ ic b ++ ")"))

        ( "spin", [ VNum period, VNum time ] ) ->
            Ok (VNum (360 * frac period time))

        ( "wave", [ VNum lo, VNum hi, VNum period, VNum time ] ) ->
            Ok (VNum (lo + (hi - lo) * (1 + sin (2 * pi * frac period time)) / 2))

        ( "zigzag", [ VNum lo, VNum hi, VNum period, VNum time ] ) ->
            Ok (VNum (lo + (hi - lo) * abs (2 * frac period time - 1)))

        _ ->
            Err ("bad arguments to Playground." ++ name)


{-| The fractional position (0..1) through a `period`-second cycle at the given time (ms). -}
frac : Float -> Float -> Float
frac period time =
    let
        q =
            time / (period * 1000)
    in
    q - toFloat (floor q)


ic : Float -> String
ic n =
    String.fromInt (round n)


ff : Float -> String
ff x =
    String.fromFloat x


attrS : String -> String -> Value
attrS k v =
    VCtor "Html.attr" [ VStr k, VStr v ]


{-| Wraps rendered shapes in a centred SVG canvas (y-axis points up, as in Playground). -}
pictureSvg : List Value -> Value
pictureSvg shapes =
    VCtor "Html.node"
        [ VStr "svg"
        , VList [ attrS "viewBox" "-320 -240 640 480", attrS "width" "640", attrS "height" "480" ]
        , VList (List.map renderShape shapes)
        ]


renderShape : Value -> Value
renderShape shape =
    case shape of
        VCtor "PShape" [ form, VNum x, VNum y, VNum a, VNum sc, VNum al ] ->
            VCtor "Html.node"
                [ VStr "g"
                , VList [ attrS "transform" (transformStr x y a sc), attrS "opacity" (ff al) ]
                , VList [ renderForm form ]
                ]

        _ ->
            VCtor "Html.text" [ VStr "" ]


transformStr : Float -> Float -> Float -> Float -> String
transformStr x y a sc =
    "translate(" ++ ff x ++ " " ++ ff (negate y) ++ ") rotate(" ++ ff (negate a) ++ ") scale(" ++ ff sc ++ ")"


renderForm : Value -> Value
renderForm form =
    case form of
        VCtor "PCircle" [ VStr color, VNum r ] ->
            VCtor "Html.node" [ VStr "ellipse", VList [ attrS "cx" "0", attrS "cy" "0", attrS "rx" (ff r), attrS "ry" (ff r), attrS "fill" color ], VList [] ]

        VCtor "POval" [ VStr color, VNum w, VNum h ] ->
            VCtor "Html.node" [ VStr "ellipse", VList [ attrS "cx" "0", attrS "cy" "0", attrS "rx" (ff (w / 2)), attrS "ry" (ff (h / 2)), attrS "fill" color ], VList [] ]

        VCtor "PRect" [ VStr color, VNum w, VNum h ] ->
            VCtor "Html.node" [ VStr "rect", VList [ attrS "x" (ff (negate (w / 2))), attrS "y" (ff (negate (h / 2))), attrS "width" (ff w), attrS "height" (ff h), attrS "fill" color ], VList [] ]

        VCtor "PNgon" [ VStr color, VNum n, VNum r ] ->
            VCtor "Html.node" [ VStr "path", VList [ attrS "d" (ngonPath n r), attrS "fill" color ], VList [] ]

        VCtor "PWords" [ VStr color, VStr s ] ->
            VCtor "Html.node" [ VStr "text_", VList [ attrS "x" "0", attrS "y" "0", attrS "text-anchor" "middle", attrS "fill" color ], VList [ VCtor "Html.text" [ VStr s ] ] ]

        VCtor "PImage" [ VNum w, VNum h, VStr url ] ->
            VCtor "Html.node" [ VStr "image", VList [ attrS "x" (ff (negate (w / 2))), attrS "y" (ff (negate (h / 2))), attrS "width" (ff w), attrS "height" (ff h), attrS "href" url ], VList [] ]

        _ ->
            VCtor "Html.text" [ VStr "" ]


-- elm-playground `game`: a Computer-driven loop the editor renders and steps.


{-| A boolean keyboard field as 0.0/1.0 (used by `toX`/`toY`). -}
boolField : String -> Value -> Float
boolField name kb =
    case kb of
        VRecord fields ->
            case lookup name fields of
                Just (VBool True) ->
                    1

                _ ->
                    0

        _ ->
            0


{-| The `Computer` a game's `view`/`update` receive: mouse, keyboard, screen and time. The keyboard
flags are derived from the set of currently-pressed key names; time is milliseconds. -}
computerValue : List String -> Float -> Value
computerValue keys time =
    let
        down k =
            VBool (List.member k keys)

        on names =
            VBool (List.any (\k -> List.member k keys) names)
    in
    VRecord
        [ ( "mouse", VRecord [ ( "x", VNum 0 ), ( "y", VNum 0 ), ( "down", VBool False ) ] )
        , ( "keyboard"
          , VRecord
                [ ( "up", on [ "ArrowUp", "w", "W" ] )
                , ( "down", on [ "ArrowDown", "s", "S" ] )
                , ( "left", on [ "ArrowLeft", "a", "A" ] )
                , ( "right", on [ "ArrowRight", "d", "D" ] )
                , ( "space", down " " )
                , ( "enter", down "Enter" )
                , ( "shift", down "Shift" )
                , ( "keys", VList (List.map VStr keys) )
                ]
          )
        , ( "screen"
          , VRecord
                [ ( "width", VNum 640 ), ( "height", VNum 480 ), ( "top", VNum 240 ), ( "bottom", VNum -240 ), ( "left", VNum -320 ), ( "right", VNum 320 ) ]
          )
        , ( "time", VNum time )
        ]


{-| Extracts a game's (view, update, memory) from the project's `main`, if it is a `game`. -}
gameOf : Resolve -> List ( String, String ) -> Maybe ( Value, Value, Value )
gameOf mainValue files =
    case mainValue files of
        Ok (VCtor "Playground.game" [ view, update, mem ]) ->
            Just ( view, update, mem )

        _ ->
            Nothing


{-| The view of a `Playground.animation` main, if the project is one. -}
animationOf : Resolve -> List ( String, String ) -> Maybe Value
animationOf mainValue files =
    case mainValue files of
        Ok (VCtor "Playground.animation" [ view ]) ->
            Just view

        _ ->
            Nothing


{-| A game's initial memory (the third argument to `game`), if the project is a game; for an
`animation` a marker memory, so the editor's frame loop (which gates on `gameMem`) drives it too. -}
gameInitMem : Resolve -> List ( String, String ) -> Maybe Value
gameInitMem mainValue files =
    case gameOf mainValue files of
        Just ( _, _, mem ) ->
            Just mem

        Nothing ->
            animationOf mainValue files |> Maybe.map (\_ -> VCtor "$Anim" [])


{-| Renders a game's `view computer memory` (or an `animation`'s `view time`) to SVG, for the given
keys and time. -}
gameView : Resolve -> ApplyTo -> List ( String, String ) -> List String -> Float -> Value -> Result String Value
gameView mainValue applyValue files keys time mem =
    case ( parseProject files, gameOf mainValue files ) of
        ( Ok globals, Just ( view, _, _ ) ) ->
            applyValue globals view (computerValue keys time)
                |> Result.andThen (\f -> applyValue globals f mem)
                |> Result.andThen (shapesToSvg "game view")

        ( Ok globals, Nothing ) ->
            case animationOf mainValue files of
                Just view ->
                    applyValue globals view (VNum time) |> Result.andThen (shapesToSvg "animation view")

                Nothing ->
                    Err "not a game"

        _ ->
            Err "not a game"


{-| Renders a list-of-shapes value to an SVG picture, or reports a type error. -}
shapesToSvg : String -> Value -> Result String Value
shapesToSvg what shapes =
    case shapes of
        VList ss ->
            Ok (pictureSvg ss)

        _ ->
            Err (what ++ " must return a list of shapes")


{-| Steps a game's `update computer memory` to the next memory; an animation has no state to step. -}
gameStep : Resolve -> ApplyTo -> List ( String, String ) -> List String -> Float -> Value -> Result String Value
gameStep mainValue applyValue files keys time mem =
    case gameOf mainValue files of
        Just ( _, update, _ ) ->
            case parseProject files of
                Ok globals ->
                    applyValue globals update (computerValue keys time)
                        |> Result.andThen (\f -> applyValue globals f mem)

                Err e ->
                    Err e

        Nothing ->
            Ok mem -- an animation: the view depends only on the (externally advanced) time


ngonPath : Float -> Float -> String
ngonPath n r =
    let
        pts =
            List.map
                (\i ->
                    let
                        ang =
                            2 * pi * toFloat i / n - pi / 2
                    in
                    { px = r * cos ang, py = r * sin ang }
                )
                (List.range 0 (round n - 1))
    in
    case pts of
        [] ->
            ""

        p0 :: rest ->
            "M " ++ ff p0.px ++ " " ++ ff p0.py ++ String.join "" (List.map (\p -> " L " ++ ff p.px ++ " " ++ ff p.py) rest) ++ " Z"


{-| The Playground named colours (approximate hex). -}
playgroundColor : String -> Maybe String
playgroundColor name =
    case name of
        "red" ->
            Just "#cc0000"

        "orange" ->
            Just "#f57900"

        "yellow" ->
            Just "#edd400"

        "green" ->
            Just "#4e9a06"

        "blue" ->
            Just "#3465a4"

        "purple" ->
            Just "#75507b"

        "brown" ->
            Just "#8f5902"

        "black" ->
            Just "#000000"

        "white" ->
            Just "#ffffff"

        "lightGray" ->
            Just "#d3d7cf"

        "gray" ->
            Just "#babdb6"

        "darkGray" ->
            Just "#888a85"

        "charcoal" ->
            Just "#2e3436"

        "lightBlue" ->
            Just "#729fcf"

        "lightGreen" ->
            Just "#8ae234"

        "lightYellow" ->
            Just "#fce94f"

        "darkRed" ->
            Just "#a40000"

        "darkGreen" ->
            Just "#4e9a06"

        "darkBlue" ->
            Just "#204a87"

        "lightPurple" ->
            Just "#ad7fa8"

        "darkPurple" ->
            Just "#5c3566"

        "lightRed" ->
            Just "#ef2929"

        "lightOrange" ->
            Just "#fcaf3e"

        "darkOrange" ->
            Just "#ce5c00"

        "darkYellow" ->
            Just "#c4a000"

        "lightBrown" ->
            Just "#e9b96e"

        "darkBrown" ->
            Just "#8f5902"

        "lightCharcoal" ->
            Just "#888a85"

        "darkCharcoal" ->
            Just "#202325"

        "grey" ->
            Just "#babdb6"

        "lightGrey" ->
            Just "#d3d7cf"

        "darkGrey" ->
            Just "#888a85"

        _ ->
            Nothing
