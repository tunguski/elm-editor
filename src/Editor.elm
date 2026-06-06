module Editor exposing (program, Model, Msg)

{-| A reusable, embeddable code playground: configure it with a list of example **URLs**, which it
fetches over HTTP at startup and presents as editable files (alongside a built-in starter so it is
never empty). It renders an editable file browser plus the live result of the selected file's
`main`. The interpreter (Lang/Lexer/Parser/Eval) does the work; this module is the UI plus the
loading and the wiring of interpreted click handlers back through a Browser.sandbox-style `update`.

Each file is an independent program: the editor always evaluates and renders the **`main`** of the
selected file — a static `Html` value, a `Browser.sandbox`/`Browser.element` app (rendered live and
interactive), or a plain value (shown as text). There is no entry-expression box and no choosing
other functions, by design. Reuse it elsewhere with `Editor.program myExampleUrls`.
-}

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Eval exposing (appAnimation, appInit, appInitCmd, appSubHandler, appSubscription, appUpdate, appUpdateCmd, appView, applyHandler, applyMsgIn, fileSelectCmd, fileSelected, gameInitMem, gameStep, gameView, hasApp, httpCmd, httpResult, lookup, mainValue, randomCmd, renderValue, runEventDecoder, taskResult)
import File
import Json.Decode as Decode
import Set exposing (Set)
import Html exposing (Html, a, button, div, input, li, node, pre, span, text, textarea, ul)
import Html.Attributes exposing (class, classList, href, placeholder, style, title, value)
import Html.Events exposing (onClick, onInput, onMouseDown, on, preventDefaultOn)
import Highlight
import Assist
import Share
import Storage
import Http
import Lang exposing (Value(..))
import Task
import Time
import WebGL


type alias Model =
    { files : List ( String, String )
    , libs : List ( String, String ) -- optional hidden library modules merged into the eval scope
    , selected : String
    , app : Result String Value
    , newName : String
    , seed : Int
    , gameMem : Maybe Value
    , gameKeys : Set String
    , gameTime : Float
    , gameError : Maybe String -- a runtime error from the last `update`, so the loop reports it rather than silently freezing
    , history : List Value -- successive app models (time-travel debugger)
    , msgLog : List Value -- the message that produced each model transition (msgLog[k] -> history[k+1])
    , historyAt : Int -- the index currently shown (last = live)
    , caret : Int -- the textarea's caret offset (for autocomplete)
    , completions : List String -- live autocomplete candidates for the word at the caret
    , shareText : String -- the encoded share string (shown to copy, or pasted in to restore)
    , restored : Bool -- True once a permalink (#hash) session has replaced the defaults
    , collapsed : Set String -- folder groups (see `folderOf`) the user has collapsed in the file list
    , sidebarWidth : Maybe Float -- px width override for the file sidebar (Nothing = CSS default)
    , resultWidth : Maybe Float -- px width override for the result column (Nothing = CSS default)
    , drag : Maybe Drag -- an in-progress pane-divider drag, if any
    }


{-| A live drag of a pane divider: which one, the pointer x where it began, and the dragged pane's
width then — so a move resizes by the pointer's delta from the start. -}
type alias Drag =
    { divider : Divider
    , startX : Float
    , startW : Float
    }


{-| The two resizable boundaries: between the file sidebar and the code, and between the code and the
result. Dragging the sidebar one widens the sidebar; dragging the result one widens the result. -}
type Divider
    = SidebarDivider
    | ResultDivider


type Msg
    = SelectFile String
    | EditAt String Int
    | AcceptCompletion String
    | DismissCompletions
    | SetNewName String
    | AddFile
    | OpenFile
    | OpenedFile String String
    | RemoveFile String
    | ToggleGroup String
    | ToggleAll
    | Interp Value
    | Rewind Int
    | Tick Int
    | KeyDown String
    | KeyUp String
    | Frame Float
    | AnimFrame Float
    | AppKey Bool String
    | AppResize Int Int
    | AppMouse Float Float
    | HttpResult Value (Result Http.Error String)
    | Loaded String (Result Http.Error String)
    | LoadedLib String (Result Http.Error String)
    | FilePicked Value Bool String String
    | Share
    | ShareInput String
    | Restore
    | GotHash String
    | LoadedSession (Maybe String)
    | GoBack
    | Scroll ScrollDir
    | DragStart Divider Float Float
    | DragMove Float
    | DragEnd
    | NoOp


{-| Keyboard scrolling of the code pane: a page up/down, or a jump to the top/bottom of the file. -}
type ScrollDir
    = ScrollPageUp
    | ScrollPageDown
    | ScrollTop
    | ScrollBottom


{-| A built-in starter file so the editor is usable immediately (and offline / before fetches). -}
starter : ( String, String )
starter =
    ( "Buttons.elm"
    , "module Main exposing (main)\n\nimport Browser\nimport Html exposing (button, div, text)\nimport Html.Events exposing (onClick)\n\nmain = Browser.sandbox { init = init, update = update, view = view }\n\ninit = 0\n\nupdate msg model =\n    case msg of\n        Increment ->\n            model + 1\n\n        Decrement ->\n            model - 1\n\nview model =\n    div []\n        [ button [ onClick Decrement ] [ text \"-\" ]\n        , div [] [ text (String.fromInt model) ]\n        , button [ onClick Increment ] [ text \"+\" ]\n        ]\n"
    )


{-| Builds an editor that fetches each example URL at startup and lets the user edit them. -}
program : List String -> Program () Model Msg
program urls =
    Browser.element
        { init = \_ -> ( initModel, Cmd.batch [ Browser.Navigation.getHash GotHash, Storage.load sessionKey LoadedSession, fetchAll urls, fetchLibs scriptingLibs ] )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


{-| Library modules fetched into `model.libs` at startup and merged into every file's evaluation
scope (hidden from the file list). Empty by default: the scripting libraries (Awk/M4/Csv) target
`elm script` text generation and use full-stdlib features the small in-browser interpreter doesn't
model, so they aren't bundled into the Html playground. The plumbing remains for embedding helpers. -}
scriptingLibs : List String
scriptingLibs =
    []


fetchLibs : List String -> Cmd Msg
fetchLibs urls =
    Cmd.batch (List.map (\url -> Http.get { url = url, expect = Http.expectString (LoadedLib (baseName url)) }) urls)


{-| Wires live effects: pane-divider dragging (whenever a drag is in progress) plus the selected
file's own subscriptions (a `game`'s loop, an app's keyboard/mouse, or a `Time.every` tick). -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ dragSubscription model, fileSubscriptions model ]


{-| While a divider is held, follow the pointer (document-wide, so it keeps tracking past the bar)
and release on mouse-up. -}
dragSubscription : Model -> Sub Msg
dragSubscription model =
    case model.drag of
        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map DragMove (Decode.field "clientX" Decode.float))
                , Browser.Events.onMouseUp (Decode.succeed DragEnd)
                ]

        Nothing ->
            Sub.none


fileSubscriptions : Model -> Sub Msg
fileSubscriptions model =
    case model.gameMem of
        Just _ ->
            Sub.batch
                [ Browser.Events.onKeyDown (Decode.map KeyDown (Decode.field "key" Decode.string))
                , Browser.Events.onKeyUp (Decode.map KeyUp (Decode.field "key" Decode.string))
                , Browser.Events.onAnimationFrameDelta Frame
                ]

        Nothing ->
            case model.app of
                Ok m ->
                    let
                        files =
                            selectedFile model

                        -- A live animation-frame loop, for apps subscribing via onAnimationFrameDelta.
                        animSub =
                            case appAnimation files m of
                                Just _ ->
                                    Browser.Events.onAnimationFrameDelta AnimFrame

                                Nothing ->
                                    Sub.none

                        -- Keyboard subscriptions for Browser.element apps (e.g. first-person's WASD),
                        -- not just playground games: the app's decoder runs against the real key event.
                        keyDownSub =
                            case appSubHandler files m "Sub.keyDown" of
                                Just _ ->
                                    Browser.Events.onKeyDown (Decode.map (AppKey True) (Decode.field "key" Decode.string))

                                Nothing ->
                                    Sub.none

                        keyUpSub =
                            case appSubHandler files m "Sub.keyUp" of
                                Just _ ->
                                    Browser.Events.onKeyUp (Decode.map (AppKey False) (Decode.field "key" Decode.string))

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


fetchAll : List String -> Cmd Msg
fetchAll urls =
    Cmd.batch (List.map (\url -> Http.get { url = url, expect = Http.expectString (Loaded url) }) urls)


initModel : Model
initModel =
    refreshApp
        { files = [ starter ]
        , libs = []
        , selected = Tuple.first starter
        , app = Err ""
        , newName = ""
        , seed = 1
        , gameMem = Nothing
        , gameKeys = Set.empty
        , gameTime = 0
        , gameError = Nothing
        , history = []
        , msgLog = []
        , historyAt = 0
        , caret = 0
        , completions = []
        , shareText = ""
        , restored = False
        , collapsed = Set.empty
        , sidebarWidth = Nothing
        , resultWidth = Nothing
        , drag = Nothing
        }


{-| The file name from a URL ({@code examples/Foo.elm} -> {@code Foo.elm}). -}
baseName : String -> String
baseName url =
    url |> String.split "/" |> List.reverse |> List.head |> Maybe.withDefault url


{-| The folder a file key belongs to, used to group the file list. A key with a path
({@code examples/Foo.elm}) groups under its directory ({@code examples}); a bare name the user
created or opened ({@code Foo.elm}) groups under {@code Workspace}. -}
folderOf : String -> String
folderOf key =
    let
        parts =
            String.split "/" key
    in
    if List.length parts <= 1 then
        "Workspace"

    else
        String.join "/" (List.take (List.length parts - 1) parts)


{-| Display order for folder groups: the user's own files first, then the curated editor demos, then
the elm-lang.org gallery, then anything else alphabetically. -}
groupOrder : String -> Int
groupOrder folder =
    case folder of
        "Workspace" ->
            0

        "editor" ->
            1

        "examples" ->
            2

        _ ->
            3


{-| The file list grouped by folder, each group's files sorted by base name, the groups themselves in
{@code groupOrder}. Drives both the foldable sidebar and the "toggle all" button. -}
groupedFiles : Model -> List ( String, List ( String, String ) )
groupedFiles model =
    let
        folders =
            model.files
                |> List.map (Tuple.first >> folderOf)
                |> Set.fromList
                |> Set.toList
                |> List.sortBy (\g -> ( groupOrder g, g ))
    in
    List.map
        (\g ->
            ( g
            , model.files
                |> List.filter (\f -> folderOf (Tuple.first f) == g)
                |> List.sortBy (Tuple.first >> baseName)
            )
        )
        folders


{-| The file set evaluated for the selected file: the selected file first (so its definitions win),
followed by the bundled scripting libraries — which define no `main`, so they only add `import`-able
definitions to the scope. -}
selectedFile : Model -> List ( String, String )
selectedFile model =
    ( model.selected, lookup model.selected model.files |> Maybe.withDefault "" ) :: model.libs


{-| Re-initialises the running app from the selected file (its model becomes `init`) when that file
is a Browser.sandbox-style program; otherwise the app slot is unused. -}
refreshApp : Model -> Model
refreshApp model =
    let
        app =
            if hasApp (selectedFile model) then
                appInit (selectedFile model)

            else
                Err ""
    in
    { model
        | app = app
        , gameMem = gameInitMem (selectedFile model)
        , gameKeys = Set.empty
        , gameTime = 0
        , gameError = Nothing
        , history = app |> Result.map (\m -> [ m ]) |> Result.withDefault []
        , msgLog = []
        , historyAt = 0
    }


{-| Records the app's next model (and the message that produced it) in the time-travel history
(capped) and jumps the cursor to the new state. -}
recordModel : Model -> Value -> Value -> Model
recordModel model msg m =
    let
        hist =
            List.take 200 (model.history ++ [ m ])

        log =
            List.take 199 (model.msgLog ++ [ msg ])
    in
    { model | app = Ok m, history = hist, msgLog = log, historyAt = List.length hist - 1 }


{-| The app model currently shown — the one the history cursor points at (live = the last). -}
shownModel : Model -> Result String Value
shownModel model =
    case nth model.historyAt model.history of
        Just m ->
            Ok m

        Nothing ->
            model.app


nth : Int -> List a -> Maybe a
nth i xs =
    List.head (List.drop i xs)


{-| Runs one interpreted message through `update`, then handles the command it produces:
`Random.generate` is sampled with the editor's seed and the generated message dispatched (so
`Roll`-style buttons randomise); `Http.get` issues a real request whose response is fed back. `fuel`
bounds the command-chasing in case an app loops. -}
stepApp : Int -> Model -> Value -> ( Model, Cmd Msg )
stepApp fuel model interpMsg =
    case shownModel model of
        Ok m ->
            -- Continue from whatever state is shown; if rewound, drop the (now-superseded) future.
            let
                truncated =
                    { model
                        | history = List.take (model.historyAt + 1) model.history
                        , msgLog = List.take model.historyAt model.msgLog
                    }
            in
            case appUpdateCmd (selectedFile model) interpMsg m of
                Ok ( m2, cmd ) ->
                    runCmd fuel (recordModel truncated interpMsg m2) cmd

                Err e ->
                    ( { model | app = Err e }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )


{-| Handles an interpreted command: a `Random.generate` is resolved synchronously and its message
re-dispatched; an `Http.get` becomes a real request (its response comes back as `HttpResult`);
anything else is inert. -}
runCmd : Int -> Model -> Value -> ( Model, Cmd Msg )
runCmd fuel model cmd =
    case randomCmd (selectedFile model) model.seed cmd of
        Just ( genMsg, seed2 ) ->
            if fuel <= 0 then
                ( { model | seed = seed2 }, Cmd.none )

            else
                stepApp (fuel - 1) { model | seed = seed2 } genMsg

        Nothing ->
            case httpCmd cmd of
                Just ( url, toMsg ) ->
                    ( model
                    , Http.get { url = url, expect = Http.expectString (HttpResult toMsg) }
                    )

                Nothing ->
                    case taskResult (selectedFile model) cmd of
                        Just (Ok stepMsg) ->
                            if fuel <= 0 then
                                ( model, Cmd.none )

                            else
                                stepApp (fuel - 1) model stepMsg

                        Just (Err _) ->
                            ( model, Cmd.none )

                        Nothing ->
                            case fileSelectCmd cmd of
                                Just ( toMsg, many ) ->
                                    -- Open a real browser file picker; the choice returns via FilePicked.
                                    ( model, File.openPicker (\name content -> FilePicked toMsg many name content) )

                                Nothing ->
                                    ( model, Cmd.none )


{-| Refreshes the running app/game from the selected file and issues its `init` command (so a
`Browser.element` that fetches on startup — like the book example — actually loads). -}
refreshAndRun : Model -> ( Model, Cmd Msg )
refreshAndRun model =
    let
        m =
            refreshApp model
    in
    case appInitCmd (selectedFile m) of
        Ok ( _, cmd ) ->
            runCmd 100 m cmd

        Err _ ->
            ( m, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectFile name ->
            refreshAndRun { model | selected = name }

        EditAt content caret ->
            -- Recompute autocomplete candidates for the word the caret sits in, then re-run the file.
            let
                word =
                    Assist.wordAt content caret
            in
            withAutosave
                (refreshAndRun
                    { model
                        | files = setFile model.selected content model.files
                        , caret = caret
                        , completions = Assist.completions content word
                    }
                )

        AcceptCompletion choice ->
            -- Replace the half-typed word with the chosen completion.
            let
                source =
                    lookup model.selected model.files |> Maybe.withDefault ""

                ( newSource, newCaret ) =
                    Assist.accept source model.caret choice
            in
            refreshAndRun
                { model
                    | files = setFile model.selected newSource model.files
                    , caret = newCaret
                    , completions = []
                }

        DismissCompletions ->
            ( { model | completions = [] }, Cmd.none )

        Share ->
            -- Encode the session into a string to copy, and into the URL fragment for a permalink.
            let
                encoded =
                    Share.encodeFiles model.files
            in
            ( { model | shareText = encoded }, Browser.Navigation.setHash encoded )

        ShareInput text ->
            ( { model | shareText = text }, Cmd.none )

        GotHash hash ->
            -- A permalink (#hash) opened the editor: restore that session in place of the defaults.
            restoreSession hash model

        LoadedSession stored ->
            -- An autosaved session in localStorage: restore it, unless a permalink already won.
            case stored of
                Just text ->
                    if model.restored then
                        ( model, Cmd.none )

                    else
                        restoreSession text model

                Nothing ->
                    ( model, Cmd.none )

        Restore ->
            -- Replace the session with the files decoded from the pasted share string.
            case Share.decodeFiles model.shareText of
                [] ->
                    ( model, Cmd.none )

                files ->
                    refreshAndRun
                        { model | files = files, selected = Tuple.first (firstFile files) }

        SetNewName n ->
            ( { model | newName = n }, Cmd.none )

        AddFile ->
            let
                name =
                    if String.endsWith ".elm" model.newName then
                        model.newName

                    else
                        model.newName ++ ".elm"
            in
            if model.newName == "" || hasFile name model.files then
                ( model, Cmd.none )

            else
                refreshAndRun { model | files = model.files ++ [ ( name, "main = text \"new file\"" ) ], selected = name, newName = "" }

        OpenFile ->
            -- Open a local .elm from disk via the browser file picker; its contents arrive as OpenedFile.
            ( model, File.openPicker (\name content -> OpenedFile name content) )

        OpenedFile name content ->
            let
                unique =
                    if hasFile name model.files then
                        "imported-" ++ name

                    else
                        name
            in
            refreshAndRun { model | files = model.files ++ [ ( unique, content ) ], selected = unique }

        RemoveFile name ->
            let
                remaining =
                    List.filter (\f -> Tuple.first f /= name) model.files
            in
            refreshAndRun
                { model
                    | files = remaining
                    , selected =
                        if model.selected == name then
                            remaining |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault ""

                        else
                            model.selected
                }

        Interp interpMsg ->
            stepApp 100 model interpMsg

        Rewind i ->
            ( { model | historyAt = clamp 0 (List.length model.history - 1) i }, Cmd.none )

        Tick t ->
            -- A Time.every tick: feed the subscription's message (toMsg (millisToPosix t)) to update.
            case model.app of
                Ok m ->
                    case appSubscription (selectedFile model) m of
                        Just ( _, toMsg ) ->
                            case applyMsgIn (selectedFile model) toMsg (VNum (toFloat t)) of
                                Ok interpMsg ->
                                    stepApp 100 model interpMsg

                                Err _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        HttpResult toMsg result ->
            -- A real HTTP request finished: build the interpreted message and feed it to `update`.
            case httpResult (selectedFile model) toMsg (Result.toMaybe result) of
                Ok interpMsg ->
                    stepApp 100 model interpMsg

                Err _ ->
                    ( model, Cmd.none )

        FilePicked toMsg many name content ->
            -- The user chose a file: apply the program's File handler to it and step the app.
            case fileSelected (selectedFile model) toMsg many name content of
                Ok interpMsg ->
                    stepApp 100 model interpMsg

                Err _ ->
                    ( model, Cmd.none )

        AnimFrame dt ->
            -- An animation frame for a Browser.element app subscribing via onAnimationFrameDelta:
            -- apply its toMsg to the frame delta (ms) and step the app, so animated scenes advance.
            case shownModel model of
                Ok m ->
                    case appAnimation (selectedFile model) m of
                        Just toMsg ->
                            case applyMsgIn (selectedFile model) toMsg (VNum dt) of
                                Ok interpMsg ->
                                    stepApp 100 model interpMsg

                                Err _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AppKey isDown key ->
            -- A real key event for a Browser.element app: run the subscription's decoder against a
            -- `{ "key": … }` event and dispatch the message it produces (e.g. first-person's WASD).
            case shownModel model of
                Ok m ->
                    case appSubHandler (selectedFile model) m (if isDown then "Sub.keyDown" else "Sub.keyUp") of
                        Just decoder ->
                            case runEventDecoder (selectedFile model) decoder (keyEventJson key) of
                                Ok interpMsg ->
                                    stepApp 100 model interpMsg

                                Err _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AppMouse x y ->
            -- A mouse move for a Browser.element app: run the sub's decoder against the event.
            case shownModel model of
                Ok m ->
                    case appSubHandler (selectedFile model) m "Sub.mouseMove" of
                        Just decoder ->
                            case runEventDecoder (selectedFile model) decoder (mouseEventJson x y) of
                                Ok interpMsg ->
                                    stepApp 100 model interpMsg

                                Err _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AppResize w h ->
            -- A window resize for a Browser.element app: apply the sub's (Int -> Int -> msg) toMsg.
            case shownModel model of
                Ok m ->
                    case appSubHandler (selectedFile model) m "Sub.resize" of
                        Just toMsg ->
                            case
                                applyMsgIn (selectedFile model) toMsg (VNum (toFloat w))
                                    |> Result.andThen (\partial -> applyMsgIn (selectedFile model) partial (VNum (toFloat h)))
                            of
                                Ok interpMsg ->
                                    stepApp 100 model interpMsg

                                Err _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        KeyDown key ->
            ( { model | gameKeys = Set.insert key model.gameKeys }, Cmd.none )

        KeyUp key ->
            ( { model | gameKeys = Set.remove key model.gameKeys }, Cmd.none )

        Frame dt ->
            -- Advance the game one animation frame: `update computer memory` with the held keys.
            case model.gameMem of
                Just mem ->
                    let
                        time =
                            model.gameTime + dt
                    in
                    case gameStep (selectedFile model) (Set.toList model.gameKeys) time mem of
                        Ok mem2 ->
                            ( { model | gameMem = Just mem2, gameTime = time, gameError = Nothing }, Cmd.none )

                        Err e ->
                            -- `update` errored on this state: surface it instead of silently keeping
                            -- the old memory (which looks like the game has frozen for no reason).
                            ( { model | gameTime = time, gameError = Just e }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Loaded url result ->
            if model.restored then
                -- A permalink session is active; ignore the default examples still arriving over HTTP.
                ( model, Cmd.none )

            else
                case result of
                    Ok content ->
                        -- Add (or refresh) the fetched example as an editable file, keyed by its full
                        -- path (e.g. `examples/Hello.elm`) so files with the same base name in
                        -- different folders stay distinct and group by folder in the sidebar.
                        let
                            name =
                                url

                            files =
                                if hasFile name model.files then
                                    setFile name content model.files

                                else
                                    model.files ++ [ ( name, content ) ]
                        in
                        ( refreshApp { model | files = files }, Cmd.none )

                    Err _ ->
                        ( model, Cmd.none )

        LoadedLib name result ->
            -- A bundled scripting library arrived: keep it in `libs` (merged into scope, hidden from
            -- the file list) and re-run so a program that imports it picks it up.
            case result of
                Ok content ->
                    let
                        libs =
                            if hasFile name model.libs then
                                setFile name content model.libs

                            else
                                model.libs ++ [ ( name, content ) ]
                    in
                    ( refreshApp { model | libs = libs }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GoBack ->
            -- Return to the previous page when the visitor came from elsewhere on the site (the side
            -- menu links here from other pages); otherwise go to the gallery home page.
            ( model, Browser.Navigation.backOr "index.html" )

        Scroll dir ->
            -- PageDown/PageUp/Home/End scroll the code pane (read its viewport, then re-position it).
            ( model, scrollCode dir )

        DragStart which x w ->
            -- A divider grab: remember where the pointer started and how wide the pane was then.
            ( { model | drag = Just { divider = which, startX = x, startW = w } }, Cmd.none )

        DragMove x ->
            -- The pointer moved while dragging: resize the pane by its delta from the grab point.
            case model.drag of
                Just d ->
                    let
                        delta =
                            x - d.startX
                    in
                    case d.divider of
                        SidebarDivider ->
                            ( { model | sidebarWidth = Just (clamp 140 640 (d.startW + delta)) }, Cmd.none )

                        ResultDivider ->
                            -- The result pane sits on the right, so dragging left (negative delta) widens it.
                            ( { model | resultWidth = Just (clamp 200 900 (d.startW - delta)) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DragEnd ->
            ( { model | drag = Nothing }, Cmd.none )

        ToggleGroup folder ->
            -- Fold/unfold a single folder group in the file list.
            ( { model
                | collapsed =
                    if Set.member folder model.collapsed then
                        Set.remove folder model.collapsed

                    else
                        Set.insert folder model.collapsed
              }
            , Cmd.none
            )

        ToggleAll ->
            -- One button to fold/unfold every group: if anything is open, collapse all; else expand all.
            let
                groups =
                    List.map Tuple.first (groupedFiles model)
            in
            ( { model
                | collapsed =
                    if List.all (\g -> Set.member g model.collapsed) groups then
                        Set.empty

                    else
                        Set.fromList groups
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


{-| The localStorage key the session is autosaved under. -}
sessionKey : String
sessionKey =
    "elm-editor-session"


{-| Tacks an autosave of the model's files onto a command, so edits survive a page reload. -}
withAutosave : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withAutosave ( m, cmd ) =
    ( m, Cmd.batch [ cmd, Storage.save sessionKey (Share.encodeFiles m.files) ] )


{-| Replaces the session with the files encoded in `text` (a permalink or autosaved string), marking
the session restored so the default examples (and a later autosave load) don't clobber it. -}
restoreSession : String -> Model -> ( Model, Cmd Msg )
restoreSession text model =
    case Share.decodeFiles text of
        [] ->
            ( model, Cmd.none )

        files ->
            refreshAndRun
                { model | files = files, selected = Tuple.first (firstFile files), restored = True }


{-| The first file of a session (a safe fallback when a restored session is somehow empty). -}
firstFile : List ( String, String ) -> ( String, String )
firstFile files =
    case files of
        f :: _ ->
            f

        [] ->
            ( "Main.elm", "" )


setFile : String -> String -> List ( String, String ) -> List ( String, String )
setFile name content files =
    List.map
        (\f ->
            if Tuple.first f == name then
                ( name, content )

            else
                f
        )
        files


hasFile : String -> List ( String, String ) -> Bool
hasFile name files =
    List.any (\f -> Tuple.first f == name) files



-- VIEW


{-| A share/restore bar: "Share" encodes the whole session into the text box (copy it to share);
pasting a shared string and pressing "Restore" replaces the session with it. Pure (no ports). -}
shareBar : Model -> Html Msg
shareBar model =
    div [ class "ed-sharebar" ]
        [ button [ onClick Share ] [ text "Share" ]
        , input
            [ value model.shareText
            , onInput ShareInput
            , placeholder "paste a shared session…"
            , class "ed-share-input"
            ]
            []
        , button [ onClick Restore ] [ text "Restore" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "ed-root" ]
        [ div [ class "ed-header" ]
            [ backLink
            , span [ class "ed-title" ] [ text "Elm-in-Elm playground" ]
            , span [ class "ed-tagline" ] [ text "files · code · live result" ]
            , shareBar model
            ]
        , div [ classList [ ( "ed-body", True ), ( "ed-dragging", model.drag /= Nothing ) ] ]
            [ fileSidebar model
            , divider SidebarDivider
            , codeColumn model
            , divider ResultDivider
            , resultColumn model
            ]
        ]


{-| A draggable bar between two panes. Grabbing it (mousedown) reads the adjacent pane's current pixel
width straight off the DOM event, so the following moves resize from a known starting point; the
default is prevented so the drag doesn't begin a text selection. -}
divider : Divider -> Html Msg
divider which =
    div
        [ class "ed-divider"
        , preventDefaultOn "mousedown" (dragStartDecoder which)
        ]
        []


{-| Decodes a divider mousedown into `DragStart`: the pointer x, plus the resized pane's width read
from the divider's sibling (the sidebar lies before its bar; the result column after its bar). -}
dragStartDecoder : Divider -> Decode.Decoder ( Msg, Bool )
dragStartDecoder which =
    let
        widthPath =
            case which of
                SidebarDivider ->
                    [ "target", "previousElementSibling", "offsetWidth" ]

                ResultDivider ->
                    [ "target", "nextElementSibling", "offsetWidth" ]
    in
    Decode.map2 (\x w -> ( DragStart which x w, True ))
        (Decode.field "clientX" Decode.float)
        (Decode.at widthPath Decode.float)


{-| Inline width overrides for a resized pane (overriding the CSS flex-basis/max-width), or nothing
while the pane is at its default size. -}
widthStyle : Maybe Float -> List (Html.Attribute Msg)
widthStyle width =
    case width of
        Just w ->
            let
                px =
                    String.fromInt (round w) ++ "px"
            in
            [ style "flex" ("0 0 " ++ px), style "width" px, style "max-width" "none" ]

        Nothing ->
            []


{-| A themed "back" link in the header — an arrow plus the site wordmark, in the gallery's accent
colour so it reads as part of the same site. Clicking it goes *back* in history when the visitor
arrived from another page on the site (e.g. via the shared side menu), and falls back to the gallery
home page otherwise; the `href` is the no-JS fallback and the middle-click/open-in-new-tab target. -}
backLink : Html Msg
backLink =
    a
        [ href "index.html"
        , class "ed-back"
        , title "Back"
        , preventDefaultOn "click" (Decode.succeed ( GoBack, True ))
        ]
        [ text "← elm-lang" ]


{-| The middle column: the file name tab and the syntax-highlighted code editor, scrolling on its own. -}
{-| The id of the code pane's scroll container, so `Browser.Dom.getViewportOf`/`setViewportOf` can
read and move it for keyboard scrolling. -}
codeColumnId : String
codeColumnId =
    "ed-code-col"


codeColumn : Model -> Html Msg
codeColumn model =
    div [ class "ed-code-col", Html.Attributes.id codeColumnId ]
        [ div [ class "ed-code-card" ]
            [ div [ class "ed-filename" ] [ text model.selected ]
            , codeEditor model (lookup model.selected model.files |> Maybe.withDefault "")
            ]
        ]


{-| The right column (~40% of the width): the live result of the selected file's `main`, scrolling
on its own. -}
resultColumn : Model -> Html Msg
resultColumn model =
    div (class "ed-result-col" :: widthStyle model.resultWidth)
        [ mainPane model ]


{-| The syntax-highlighted code editor: a transparent `<textarea>` (which owns the caret, selection
and typing) layered over a `<pre>` of coloured `<span>`s. Both share the exact same font, padding and
wrapping, so the highlighted text underneath stays aligned with what's typed (the react-simple-code-
editor technique). The `<pre>` is in normal flow and sets the height; the textarea fills it. -}
codeEditor : Model -> String -> Html Msg
codeEditor model source =
    div [ class "ed-editor" ]
        [ errorRibbon model source
        , div [ class "ed-editor-flex" ]
            [ gutter model source
            , div [ class "ed-code-area" ]
                [ pre [ class "code-text ed-pre" ]
                    (List.map renderSegment (Highlight.segments source) ++ [ text "\n" ])
                , squiggleOverlay model source
                , textarea [ onEdit, onScrollKey, value source, class "code-text ed-textarea" ] []
                , completionBar model
                ]
            ]
        ]


{-| A line-number gutter beside the code, highlighting the line the caret is on. Aligned to the code
by sharing its font, size, line-height and top padding (long wrapped lines aside). -}
gutter : Model -> String -> Html Msg
gutter model source =
    let
        lineCount =
            List.length (String.lines source)

        current =
            currentLine source model.caret
    in
    div [ class "ed-gutter" ]
        (List.map (gutterLine current) (List.range 1 lineCount))


gutterLine : Int -> Int -> Html Msg
gutterLine current n =
    div [ classList [ ( "ed-gutter-line", True ), ( "current", n == current ) ] ]
        [ text (String.fromInt n) ]


{-| The 1-based line the caret sits on (one past the newlines before it). -}
currentLine : String -> Int -> Int
currentLine source caret =
    1 + List.length (List.filter ((==) '\n') (String.toList (String.left caret source)))


{-| A `<textarea>` input handler that captures both the new text and the caret offset
(`selectionStart`), so autocomplete knows the word being typed. -}
onEdit : Html.Attribute Msg
onEdit =
    on "input"
        (Decode.map2 EditAt
            (Decode.at [ "target", "value" ] Decode.string)
            (Decode.at [ "target", "selectionStart" ] Decode.int)
        )


{-| A `keydown` handler for the code textarea that turns PageDown/PageUp/Home/End into a `Scroll`
message and suppresses their native behaviour (which, on a full-height transparent textarea, would
jump the caret to the document edge rather than scroll one screen). Other keys fall through to normal
editing — the decoder fails, so no message is sent and the default is not prevented. -}
onScrollKey : Html.Attribute Msg
onScrollKey =
    preventDefaultOn "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    case scrollDirFor key of
                        Just dir ->
                            Decode.succeed ( Scroll dir, True )

                        Nothing ->
                            Decode.fail "not a scroll key"
                )
        )


{-| The scroll a navigation key requests, or `Nothing` for keys that should edit text as usual. -}
scrollDirFor : String -> Maybe ScrollDir
scrollDirFor key =
    case key of
        "PageDown" ->
            Just ScrollPageDown

        "PageUp" ->
            Just ScrollPageUp

        "Home" ->
            Just ScrollTop

        "End" ->
            Just ScrollBottom

        _ ->
            Nothing


{-| Scrolls the code pane: read its current viewport, then re-position it a page up/down, or all the
way to the top/bottom. A page is the visible height less a line of overlap so context is kept. -}
scrollCode : ScrollDir -> Cmd Msg
scrollCode dir =
    Browser.Dom.getViewportOf codeColumnId
        |> Task.andThen
            (\vp ->
                let
                    page =
                        max 40 (vp.viewport.height - 40)

                    y =
                        case dir of
                            ScrollPageUp ->
                                vp.viewport.y - page

                            ScrollPageDown ->
                                vp.viewport.y + page

                            ScrollTop ->
                                0

                            ScrollBottom ->
                                vp.scene.height
                in
                Browser.Dom.setViewportOf codeColumnId vp.viewport.x y
            )
        |> Task.attempt (\_ -> NoOp)


{-| The autocomplete suggestions for the word at the caret, as a click-to-insert bar. Empty (and so
invisible) when there are no candidates. -}
completionBar : Model -> Html Msg
completionBar model =
    if List.isEmpty model.completions then
        text ""

    else
        div [ class "ed-completion-bar" ]
            (List.map completionChip (List.take 12 model.completions))


completionChip : String -> Html Msg
completionChip label =
    button [ onMouseDown (AcceptCompletion label), class "ed-completion-chip" ]
        [ text label ]


{-| When the selected file fails to evaluate, surface the error — and, if it names an identifier that
appears in the source, the line/column where it is (computed by `Assist.squiggleFor`). -}
errorRibbon : Model -> String -> Html Msg
errorRibbon model source =
    case model.app of
        Err message ->
            if message == "" then
                text ""

            else
                div [ class "ed-error" ]
                    [ text ("⚠ " ++ located model source ++ message) ]

        Ok _ ->
            text ""


{-| A "line N, col C: " prefix locating the offending identifier in the source, or "" if none. -}
located : Model -> String -> String
located model source =
    case squiggle model source of
        Just loc ->
            "line " ++ String.fromInt (loc.line + 1) ++ ", col " ++ String.fromInt (loc.column + 1) ++ ": "

        Nothing ->
            ""


{-| The location to squiggle: where the current error's offending identifier first appears, if any. -}
squiggle : Model -> String -> Maybe { line : Int, column : Int, length : Int }
squiggle model source =
    case model.app of
        Err message ->
            Maybe.andThen (Assist.squiggleFor source) (Assist.errorName message)

        Ok _ ->
            Nothing


{-| An overlay aligned exactly over the highlight `<pre>` (same font/padding/wrapping) that draws a
wavy red underline under the offending identifier: the text is transparent, so the syntax-highlighted
code shows through, but the underline's own colour marks the error in place. -}
squiggleOverlay : Model -> String -> Html Msg
squiggleOverlay model source =
    case squiggle model source of
        Nothing ->
            text ""

        Just loc ->
            let
                start =
                    Assist.offsetOf loc.line loc.column source

                before =
                    String.left start source

                marked =
                    String.slice start (start + loc.length) source

                after =
                    String.dropLeft (start + loc.length) source
            in
            pre [ class "code-text ed-squiggle" ]
                [ text before
                , span [ class "ed-squiggle-mark" ] [ text marked ]
                , text after
                ]


renderSegment : ( String, String ) -> Html Msg
renderSegment ( cls, txt ) =
    span [ class (segClass cls) ] [ text txt ]


{-| The CSS class for each highlighter token kind (`""` is the default foreground). -}
segClass : String -> String
segClass cls =
    if cls == "" then
        "seg"

    else
        "seg-" ++ cls


fileSidebar : Model -> Html Msg
fileSidebar model =
    div (class "ed-files" :: widthStyle model.sidebarWidth)
        [ div [ class "ed-files-head" ]
            [ div [ class "ed-files-title" ] [ text "Files" ]
            , button [ onClick ToggleAll, class "ed-toggle-all" ] [ text "Toggle all" ]
            ]
        , div [ class "ed-file-groups" ]
            (List.map (fileGroup model) (groupedFiles model))
        , div [ class "ed-newfile-row" ]
            [ input
                [ placeholder "New.elm"
                , value model.newName
                , onInput SetNewName
                , class "ed-newfile-input"
                ]
                []
            , button [ onClick AddFile, class "ed-add-btn" ] [ text "+" ]
            ]
        , button [ onClick OpenFile, class "ed-open-btn" ] [ text "Open .elm…" ]
        ]


{-| One folder group: a clickable header (caret + folder name + file count) that folds the group,
and — unless collapsed — the group's file rows. -}
fileGroup : Model -> ( String, List ( String, String ) ) -> Html Msg
fileGroup model ( folder, files ) =
    let
        isCollapsed =
            Set.member folder model.collapsed
    in
    div [ class "ed-file-group" ]
        (div [ class "ed-group-header", onClick (ToggleGroup folder) ]
            [ span [ class "ed-group-caret" ]
                [ text
                    (if isCollapsed then
                        "▸"

                     else
                        "▾"
                    )
                ]
            , span [ class "ed-group-name" ] [ text folder ]
            , span [ class "ed-group-count" ] [ text (String.fromInt (List.length files)) ]
            ]
            :: (if isCollapsed then
                    []

                else
                    [ ul [ class "ed-file-list" ] (List.map (fileRow model.selected) files) ]
               )
        )


fileRow : String -> ( String, String ) -> Html Msg
fileRow selected file =
    let
        name =
            Tuple.first file
    in
    li [ class "ed-file-row" ]
        [ button
            [ onClick (SelectFile name)
            , classList [ ( "ed-file-btn", True ), ( "active", name == selected ) ]
            ]
            [ text (baseName name) ]
        , button [ onClick (RemoveFile name), class "ed-file-x" ] [ text "x" ]
        ]


{-| Renders the result of the selected file's `main`: a live Browser.sandbox app, a static Html
value, or a plain value as text. -}
mainPane : Model -> Html Msg
mainPane model =
    div []
        [ div [ class "ed-result-title" ] [ text "Result" ]
        , div [ class "ed-result-box" ]
            [ case model.gameMem of
                Just mem ->
                    gamePane model mem

                Nothing ->
                    if hasApp (selectedFile model) then
                        liveApp model

                    else
                        staticMain (selectedFile model)
            ]
        ]


{-| Renders a running `game`'s current frame (its `view computer memory`). -}
gamePane : Model -> Value -> Html Msg
gamePane model mem =
    case model.gameError of
        Just e ->
            -- `update` raised a runtime error and the simulation can't advance; show why instead of
            -- appearing to freeze on the last good frame.
            errorBox ("Game stopped — error in update: " ++ e)

        Nothing ->
            case gameView (selectedFile model) (Set.toList model.gameKeys) model.gameTime mem of
                Ok html ->
                    renderHtml (selectedFile model) html

                Err e ->
                    errorBox e


liveApp : Model -> Html Msg
liveApp model =
    case shownModel model of
        Err e ->
            errorBox e

        Ok appModel ->
            case appView (selectedFile model) appModel of
                Ok html ->
                    div [] [ debugBar model, renderHtml (selectedFile model) html, msgLogPanel model ]

                Err e ->
                    errorBox e


{-| The time-travel debugger: a scrubber over the recorded app models. Shown once a TEA app has
taken at least one step; dragging it re-renders an earlier state, and dispatching a message from
there continues history from that point. -}
debugBar : Model -> Html Msg
debugBar model =
    let
        last =
            List.length model.history - 1
    in
    if last < 1 then
        text ""

    else
        div [ class "ed-debugbar" ]
            [ span [ style "font-weight" "700" ] [ text "⏱ time travel" ]
            , Html.node "input"
                [ Html.Attributes.attribute "type" "range"
                , Html.Attributes.attribute "min" "0"
                , Html.Attributes.attribute "max" (String.fromInt last)
                , value (String.fromInt model.historyAt)
                , onInput (\s -> Rewind (Maybe.withDefault last (String.toInt s)))
                , class "ed-debug-range"
                ]
                []
            , span [] [ text ("msg " ++ String.fromInt model.historyAt ++ " / " ++ String.fromInt last) ]
            , button
                [ onClick (Rewind last)
                , classList [ ( "ed-debug-live", True ), ( "active", model.historyAt == last ) ]
                ]
                [ text "live" ]
            ]


{-| The message log beneath the live app: one clickable chip per dispatched message (rendered with
`renderValue`), in dispatch order. Clicking a chip rewinds to the state that message produced; the
chip for the currently-shown state is highlighted. Together with the scrubber this turns the
time-travel debugger into a proper "what message led here" view. -}
msgLogPanel : Model -> Html Msg
msgLogPanel model =
    if List.isEmpty model.msgLog then
        text ""

    else
        div [ class "ed-msglog" ]
            (span [ class "ed-msglog-label" ] [ text "messages:" ]
                :: List.indexedMap (msgChip model) model.msgLog
            )


msgChip : Model -> Int -> Value -> Html Msg
msgChip model i msg =
    button
        [ onClick (Rewind (i + 1))
        , title (renderValue msg)
        , classList [ ( "ed-msg-chip", True ), ( "active", model.historyAt == i + 1 ) ]
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
back to the editor as `Interp` messages; a non-Html value is shown via its rendering. {@code files}
is threaded so an `onInput` handler can be applied to the input string at event time. -}
renderHtml : List ( String, String ) -> Value -> Html Msg
renderHtml files v =
    case v of
        VCtor "Html.text" [ VStr s ] ->
            text s

        VCtor "Html.node" [ VStr tag, VList attrs, VList children ] ->
            node tag (List.filterMap (renderAttr files) attrs) (List.map (renderHtml files) children)

        VCtor "WebGL.scene" [ VList attrs, entities ] ->
            -- Render live: a real <canvas> whose entities are handed to the JS WebGL runtime by the
            -- `WebGL.glAttr` kernel bridge (which converts the interpreter's values to GL data).
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
            -- Apply the handler to the typed text to build the message, then dispatch it.
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
