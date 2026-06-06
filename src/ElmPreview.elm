module ElmPreview exposing (Model, Msg, spec)

{-| The elm-lang preview pane: it interprets the selected file's `main` and renders it live — a
static `Html` value, a `Browser.sandbox`/`Browser.element` app (interactive, with a time-travel
debugger), or an elm-playground `game` loop — using the in-browser interpreter (`Eval`). This is the
only module that depends on the interpreter; the `Editor` shell drives it through the `Preview.Spec`
contract, so the editor itself stays interpreter-agnostic and can host other panes.

Its `Model` is the running state (the interpreted app/game and the debugger history); its `Msg` is
the interpreted app's own messages plus the loop/effect events the shell forwards. Every function
takes the shell's `Preview.Context` (the current files), since interpreting `main` depends on the
source being edited.

@docs Model, Msg, spec

-}

import Browser.Events
import Eval exposing (appAnimation, appInit, appInitCmd, appSubHandler, appSubscription, appUpdateCmd, appView, applyHandler, applyMsgIn, fileSelectCmd, fileSelected, gameInitMem, gameStep, gameView, hasApp, httpCmd, httpResult, lookup, mainValue, randomCmd, renderValue, runEventDecoder, taskResult)
import File
import Html exposing (Html, button, div, node, pre, span, text)
import Html.Attributes exposing (class, classList, style, title, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Lang exposing (Value(..))
import Preview exposing (Context)
import Set exposing (Set)
import Time
import WebGL


{-| The running preview: the interpreted app's current model (`app`), the playground game's memory
(`gameMem`) and loop state (`gameKeys`/`gameTime`/`gameError`), the random seed, and the time-travel
debugger's recorded models (`history`) and the messages that produced them (`msgLog`/`historyAt`). -}
type alias Model =
    { app : Result String Value
    , seed : Int
    , gameMem : Maybe Value
    , gameKeys : Set String
    , gameTime : Float
    , gameError : Maybe String
    , history : List Value
    , msgLog : List Value
    , historyAt : Int
    }


{-| Messages the preview handles: an interpreted app message (`Interp`), the debugger scrubber
(`Rewind`), and the loop/effect events the shell forwards from subscriptions and commands. -}
type Msg
    = Interp Value
    | Rewind Int
    | Tick Int
    | Frame Float
    | AnimFrame Float
    | AppKey Bool String
    | AppResize Int Int
    | AppMouse Float Float
    | HttpResult Value (Result Http.Error String)
    | FilePicked Value Bool String String
    | KeyDown String
    | KeyUp String
    | NoOp


{-| The pluggable preview the `Editor` shell wires in (its `Preview.Spec`). -}
spec : Preview.Spec Model Msg
spec =
    { init = init
    , sourcesChanged = sourcesChanged
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- SOURCES


{-| The file set evaluated for the selected file: the selected file first (so its definitions win),
followed by any hidden library modules. -}
evalFiles : Context -> List ( String, String )
evalFiles ctx =
    ( ctx.selected, lookup ctx.selected ctx.files |> Maybe.withDefault "" ) :: ctx.libs


empty : Model
empty =
    { app = Err ""
    , seed = 1
    , gameMem = Nothing
    , gameKeys = Set.empty
    , gameTime = 0
    , gameError = Nothing
    , history = []
    , msgLog = []
    , historyAt = 0
    }


init : Context -> ( Model, Cmd Msg )
init ctx =
    sourcesChanged ctx empty


{-| Re-initialises the running app/game from the selected file (its model becomes `init`) and issues
its `init` command (so a `Browser.element` that fetches on startup actually loads). -}
sourcesChanged : Context -> Model -> ( Model, Cmd Msg )
sourcesChanged ctx pm =
    let
        files =
            evalFiles ctx

        app =
            if hasApp files then
                appInit files

            else
                Err ""

        refreshed =
            { pm
                | app = app
                , gameMem = gameInitMem files
                , gameKeys = Set.empty
                , gameTime = 0
                , gameError = Nothing
                , history = app |> Result.map (\m -> [ m ]) |> Result.withDefault []
                , msgLog = []
                , historyAt = 0
            }
    in
    case appInitCmd files of
        Ok ( _, cmd ) ->
            runCmd ctx 100 refreshed cmd

        Err _ ->
            ( refreshed, Cmd.none )



-- HISTORY


{-| Records the app's next model (and the message that produced it) in the time-travel history
(capped) and jumps the cursor to the new state. -}
recordModel : Model -> Value -> Value -> Model
recordModel pm msg m =
    let
        hist =
            List.take 200 (pm.history ++ [ m ])

        log =
            List.take 199 (pm.msgLog ++ [ msg ])
    in
    { pm | app = Ok m, history = hist, msgLog = log, historyAt = List.length hist - 1 }


{-| The app model currently shown — the one the history cursor points at (live = the last). -}
shownModel : Model -> Result String Value
shownModel pm =
    case nth pm.historyAt pm.history of
        Just m ->
            Ok m

        Nothing ->
            pm.app


nth : Int -> List a -> Maybe a
nth i xs =
    List.head (List.drop i xs)



-- STEPPING


{-| Runs one interpreted message through `update`, then handles the command it produces. `fuel`
bounds the command-chasing in case an app loops. -}
stepApp : Context -> Int -> Model -> Value -> ( Model, Cmd Msg )
stepApp ctx fuel pm interpMsg =
    case shownModel pm of
        Ok m ->
            let
                truncated =
                    { pm
                        | history = List.take (pm.historyAt + 1) pm.history
                        , msgLog = List.take pm.historyAt pm.msgLog
                    }
            in
            case appUpdateCmd (evalFiles ctx) interpMsg m of
                Ok ( m2, cmd ) ->
                    runCmd ctx fuel (recordModel truncated interpMsg m2) cmd

                Err e ->
                    ( { pm | app = Err e }, Cmd.none )

        Err _ ->
            ( pm, Cmd.none )


{-| Handles an interpreted command: `Random.generate` is resolved synchronously and its message
re-dispatched; `Http.get` becomes a real request (its response comes back as `HttpResult`); a
`File.select` opens a real picker; anything else is inert. -}
runCmd : Context -> Int -> Model -> Value -> ( Model, Cmd Msg )
runCmd ctx fuel pm cmd =
    case randomCmd (evalFiles ctx) pm.seed cmd of
        Just ( genMsg, seed2 ) ->
            if fuel <= 0 then
                ( { pm | seed = seed2 }, Cmd.none )

            else
                stepApp ctx (fuel - 1) { pm | seed = seed2 } genMsg

        Nothing ->
            case httpCmd cmd of
                Just ( url, toMsg ) ->
                    ( pm
                    , Http.get { url = url, expect = Http.expectString (HttpResult toMsg) }
                    )

                Nothing ->
                    case taskResult (evalFiles ctx) cmd of
                        Just (Ok stepMsg) ->
                            if fuel <= 0 then
                                ( pm, Cmd.none )

                            else
                                stepApp ctx (fuel - 1) pm stepMsg

                        Just (Err _) ->
                            ( pm, Cmd.none )

                        Nothing ->
                            case fileSelectCmd cmd of
                                Just ( toMsg, many ) ->
                                    ( pm, File.openPicker (\name content -> FilePicked toMsg many name content) )

                                Nothing ->
                                    ( pm, Cmd.none )



-- UPDATE


update : Context -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg pm =
    case msg of
        Interp interpMsg ->
            stepApp ctx 100 pm interpMsg

        Rewind i ->
            ( { pm | historyAt = clamp 0 (List.length pm.history - 1) i }, Cmd.none )

        Tick t ->
            case pm.app of
                Ok m ->
                    case appSubscription (evalFiles ctx) m of
                        Just ( _, toMsg ) ->
                            case applyMsgIn (evalFiles ctx) toMsg (VNum (toFloat t)) of
                                Ok interpMsg ->
                                    stepApp ctx 100 pm interpMsg

                                Err _ ->
                                    ( pm, Cmd.none )

                        Nothing ->
                            ( pm, Cmd.none )

                Err _ ->
                    ( pm, Cmd.none )

        HttpResult toMsg result ->
            case httpResult (evalFiles ctx) toMsg (Result.toMaybe result) of
                Ok interpMsg ->
                    stepApp ctx 100 pm interpMsg

                Err _ ->
                    ( pm, Cmd.none )

        FilePicked toMsg many name content ->
            case fileSelected (evalFiles ctx) toMsg many name content of
                Ok interpMsg ->
                    stepApp ctx 100 pm interpMsg

                Err _ ->
                    ( pm, Cmd.none )

        AnimFrame dt ->
            case shownModel pm of
                Ok m ->
                    case appAnimation (evalFiles ctx) m of
                        Just toMsg ->
                            case applyMsgIn (evalFiles ctx) toMsg (VNum dt) of
                                Ok interpMsg ->
                                    stepApp ctx 100 pm interpMsg

                                Err _ ->
                                    ( pm, Cmd.none )

                        Nothing ->
                            ( pm, Cmd.none )

                Err _ ->
                    ( pm, Cmd.none )

        AppKey isDown key ->
            case shownModel pm of
                Ok m ->
                    case
                        appSubHandler (evalFiles ctx)
                            m
                            (if isDown then
                                "Sub.keyDown"

                             else
                                "Sub.keyUp"
                            )
                    of
                        Just decoder ->
                            case runEventDecoder (evalFiles ctx) decoder (keyEventJson key) of
                                Ok interpMsg ->
                                    stepApp ctx 100 pm interpMsg

                                Err _ ->
                                    ( pm, Cmd.none )

                        Nothing ->
                            ( pm, Cmd.none )

                Err _ ->
                    ( pm, Cmd.none )

        AppMouse x y ->
            case shownModel pm of
                Ok m ->
                    case appSubHandler (evalFiles ctx) m "Sub.mouseMove" of
                        Just decoder ->
                            case runEventDecoder (evalFiles ctx) decoder (mouseEventJson x y) of
                                Ok interpMsg ->
                                    stepApp ctx 100 pm interpMsg

                                Err _ ->
                                    ( pm, Cmd.none )

                        Nothing ->
                            ( pm, Cmd.none )

                Err _ ->
                    ( pm, Cmd.none )

        AppResize w h ->
            case shownModel pm of
                Ok m ->
                    case appSubHandler (evalFiles ctx) m "Sub.resize" of
                        Just toMsg ->
                            case
                                applyMsgIn (evalFiles ctx) toMsg (VNum (toFloat w))
                                    |> Result.andThen (\partial -> applyMsgIn (evalFiles ctx) partial (VNum (toFloat h)))
                            of
                                Ok interpMsg ->
                                    stepApp ctx 100 pm interpMsg

                                Err _ ->
                                    ( pm, Cmd.none )

                        Nothing ->
                            ( pm, Cmd.none )

                Err _ ->
                    ( pm, Cmd.none )

        KeyDown key ->
            ( { pm | gameKeys = Set.insert key pm.gameKeys }, Cmd.none )

        KeyUp key ->
            ( { pm | gameKeys = Set.remove key pm.gameKeys }, Cmd.none )

        Frame dt ->
            case pm.gameMem of
                Just mem ->
                    let
                        time =
                            pm.gameTime + dt
                    in
                    case gameStep (evalFiles ctx) (Set.toList pm.gameKeys) time mem of
                        Ok mem2 ->
                            ( { pm | gameMem = Just mem2, gameTime = time, gameError = Nothing }, Cmd.none )

                        Err e ->
                            ( { pm | gameTime = time, gameError = Just e }, Cmd.none )

                Nothing ->
                    ( pm, Cmd.none )

        NoOp ->
            ( pm, Cmd.none )


{-| A minimal `keydown`/`keyup` event as JSON for an app's key decoder to run against. -}
keyEventJson : String -> String
keyEventJson key =
    "{\"key\":\"" ++ String.replace "\"" "" key ++ "\"}"


{-| A minimal `mousemove` event as JSON; `movementX`/`movementY` are 0 (the editor can't pointer-lock)
so a decoder reading them still succeeds. -}
mouseEventJson : Float -> Float -> String
mouseEventJson x y =
    "{\"pageX\":"
        ++ String.fromFloat x
        ++ ",\"pageY\":"
        ++ String.fromFloat y
        ++ ",\"movementX\":0,\"movementY\":0,\"offsetX\":"
        ++ String.fromFloat x
        ++ ",\"offsetY\":"
        ++ String.fromFloat y
        ++ "}"



-- SUBSCRIPTIONS


{-| The selected file's own live subscriptions: a game's animation loop and held keys, or an app's
keyboard/mouse/resize/`Time.every` subscriptions (each driven against the real browser event). -}
subscriptions : Context -> Model -> Sub Msg
subscriptions ctx pm =
    case pm.gameMem of
        Just _ ->
            Sub.batch
                [ Browser.Events.onKeyDown (jsonField "key" KeyDown)
                , Browser.Events.onKeyUp (jsonField "key" KeyUp)
                , Browser.Events.onAnimationFrameDelta Frame
                ]

        Nothing ->
            case pm.app of
                Ok m ->
                    let
                        files =
                            evalFiles ctx

                        animSub =
                            case appAnimation files m of
                                Just _ ->
                                    Browser.Events.onAnimationFrameDelta AnimFrame

                                Nothing ->
                                    Sub.none

                        keyDownSub =
                            case appSubHandler files m "Sub.keyDown" of
                                Just _ ->
                                    Browser.Events.onKeyDown (jsonField "key" (AppKey True))

                                Nothing ->
                                    Sub.none

                        keyUpSub =
                            case appSubHandler files m "Sub.keyUp" of
                                Just _ ->
                                    Browser.Events.onKeyUp (jsonField "key" (AppKey False))

                                Nothing ->
                                    Sub.none

                        resizeSub =
                            case appSubHandler files m "Sub.resize" of
                                Just _ ->
                                    Browser.Events.onResize AppResize

                                Nothing ->
                                    Sub.none

                        mouseSub =
                            case appSubHandler files m "Sub.mouseMove" of
                                Just _ ->
                                    Browser.Events.onMouseMove
                                        (Decode.map2 AppMouse
                                            (Decode.field "pageX" Decode.float)
                                            (Decode.field "pageY" Decode.float)
                                        )

                                Nothing ->
                                    Sub.none

                        timeSub =
                            case appSubscription files m of
                                Just ( interval, _ ) ->
                                    Time.every (toFloat interval) (\posix -> Tick (Time.posixToMillis posix))

                                Nothing ->
                                    Sub.none
                    in
                    Sub.batch [ animSub, keyDownSub, keyUpSub, resizeSub, mouseSub, timeSub ]

                Err _ ->
                    Sub.none


jsonField : String -> (String -> Msg) -> Decode.Decoder Msg
jsonField name toMsg =
    Decode.map toMsg (Decode.field name Decode.string)



-- VIEW


{-| Renders the selected file's `main`: a running game's current frame, a live (interactive,
debuggable) TEA app, or a static `Html`/plain value. -}
view : Context -> Model -> Html Msg
view ctx pm =
    div []
        [ div [ class "ed-result-title" ] [ text "Result" ]
        , div [ class "ed-result-box" ]
            [ case pm.gameMem of
                Just mem ->
                    gamePane ctx pm mem

                Nothing ->
                    if hasApp (evalFiles ctx) then
                        liveApp ctx pm

                    else
                        staticMain (evalFiles ctx)
            ]
        ]


gamePane : Context -> Model -> Value -> Html Msg
gamePane ctx pm mem =
    case pm.gameError of
        Just e ->
            errorBox ("Game stopped — error in update: " ++ e)

        Nothing ->
            case gameView (evalFiles ctx) (Set.toList pm.gameKeys) pm.gameTime mem of
                Ok html ->
                    renderHtml (evalFiles ctx) html

                Err e ->
                    errorBox e


liveApp : Context -> Model -> Html Msg
liveApp ctx pm =
    case shownModel pm of
        Err e ->
            errorBox e

        Ok appModel ->
            case appView (evalFiles ctx) appModel of
                Ok html ->
                    div [] [ debugBar pm, renderHtml (evalFiles ctx) html, msgLogPanel pm ]

                Err e ->
                    errorBox e


{-| The time-travel debugger scrubber over the recorded app models. -}
debugBar : Model -> Html Msg
debugBar pm =
    let
        last =
            List.length pm.history - 1
    in
    if last < 1 then
        text ""

    else
        div [ class "ed-debugbar" ]
            [ span [ style "font-weight" "700" ] [ text "⏱ time travel" ]
            , node "input"
                [ Html.Attributes.attribute "type" "range"
                , Html.Attributes.attribute "min" "0"
                , Html.Attributes.attribute "max" (String.fromInt last)
                , value (String.fromInt pm.historyAt)
                , onInput (\s -> Rewind (Maybe.withDefault last (String.toInt s)))
                , class "ed-debug-range"
                ]
                []
            , span [] [ text ("msg " ++ String.fromInt pm.historyAt ++ " / " ++ String.fromInt last) ]
            , button
                [ onClick (Rewind last)
                , classList [ ( "ed-debug-live", True ), ( "active", pm.historyAt == last ) ]
                ]
                [ text "live" ]
            ]


{-| One clickable chip per dispatched message; clicking rewinds to the state it produced. -}
msgLogPanel : Model -> Html Msg
msgLogPanel pm =
    if List.isEmpty pm.msgLog then
        text ""

    else
        div [ class "ed-msglog" ]
            (span [ class "ed-msglog-label" ] [ text "messages:" ]
                :: List.indexedMap (msgChip pm) pm.msgLog
            )


msgChip : Model -> Int -> Value -> Html Msg
msgChip pm i msg =
    button
        [ onClick (Rewind (i + 1))
        , title (renderValue msg)
        , classList [ ( "ed-msg-chip", True ), ( "active", pm.historyAt == i + 1 ) ]
        ]
        [ text (String.fromInt (i + 1) ++ ". " ++ renderValue msg) ]


staticMain : List ( String, String ) -> Html Msg
staticMain files =
    case mainValue files of
        Ok v ->
            renderHtml files v

        Err e ->
            errorBox e


errorBox : String -> Html Msg
errorBox e =
    pre [ class "ed-errorbox" ] [ text ("Error: " ++ e) ]


{-| Converts an interpreted Html `Value` tree into real `Html Msg`, wiring interpreted event handlers
back as `Interp` messages; a non-Html value is shown via its rendering. -}
renderHtml : List ( String, String ) -> Value -> Html Msg
renderHtml files v =
    case v of
        VCtor "Html.text" [ VStr s ] ->
            text s

        VCtor "Html.node" [ VStr tag, VList attrs, VList children ] ->
            node tag (List.filterMap (renderAttr files) attrs) (List.map (renderHtml files) children)

        VCtor "WebGL.scene" [ VList attrs, entities ] ->
            node "canvas"
                (List.filterMap (renderAttr files) attrs ++ [ WebGL.glAttr entities ])
                []

        _ ->
            text (renderValue v)


renderAttr : List ( String, String ) -> Value -> Maybe (Html.Attribute Msg)
renderAttr files v =
    case v of
        VCtor "Html.on" [ VStr "click", msg ] ->
            Just (onClick (Interp msg))

        VCtor "Html.on" [ VStr "input", handler ] ->
            Just
                (onInput
                    (\s ->
                        case applyHandler files handler s of
                            Ok msg ->
                                Interp msg

                            Err _ ->
                                NoOp
                    )
                )

        VCtor "Html.style" [ VStr k, VStr val ] ->
            Just (style k val)

        VCtor "Html.attr" [ VStr k, VStr val ] ->
            Just (Html.Attributes.attribute k val)

        VCtor "Html.attr" [ VStr k, VBool b ] ->
            if b then
                Just (Html.Attributes.attribute k k)

            else
                Nothing

        VCtor "Html.attr" [ VStr k, other ] ->
            Just (Html.Attributes.attribute k (renderValue other))

        _ ->
            Nothing
