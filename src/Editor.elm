module Editor exposing (program, Model, Msg, Config, CodeIntel)

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
import File
import Json.Decode as Decode
import Set exposing (Set)
import Html exposing (Html, a, button, div, input, li, node, pre, span, text, textarea, ul)
import Html.Attributes exposing (class, classList, href, placeholder, style, title, value)
import Html.Events exposing (onClick, onInput, onMouseDown, on, preventDefaultOn)
import Share
import Storage
import Http
import Preview
import Svg
import Svg.Attributes as SvgA
import Task


type alias Model pModel pMsg =
    { files : List ( String, String )
    , libs : List ( String, String ) -- optional hidden library modules merged into the eval scope
    , selected : String
    , preview : pModel -- the pluggable result pane's own state (opaque to the shell)
    , config : Config pModel pMsg -- the embedder's wiring (the preview Spec + the files to load)
    , newName : String
    , caret : Int -- the textarea's caret offset (for autocomplete)
    , completions : List String -- live autocomplete candidates for the word at the caret
    , shareText : String -- the encoded share string (shown to copy, or pasted in to restore)
    , restored : Bool -- True once a permalink (#hash) session has replaced the defaults
    , collapsed : Set String -- folder groups (see `folderOf`) the user has collapsed in the file list
    , sidebarWidth : Maybe Float -- px width override for the file sidebar (Nothing = CSS default)
    , resultWidth : Maybe Float -- px width override for the result column (Nothing = CSS default)
    , drag : Maybe Drag -- an in-progress pane-divider drag, if any
    , leftPanel : Maybe LeftPanel -- which activity-bar panel fills the left pane (Nothing = collapsed)
    }


{-| The content the left pane can show, chosen from the VSCode-style activity bar. Only the file list
exists today; clicking the active panel's icon collapses the pane (`leftPanel = Nothing`). -}
type LeftPanel
    = FilesPanel


{-| The embedder's configuration. The shell owns the generic IDE chrome; everything language- or
host-specific is supplied here, so the same shell hosts the Elm playground, a CSS theme builder, a
Vega editor, … The shell stays generic over the preview's `pModel`/`pMsg`.

  - `preview` — the pluggable result pane (`Preview.Spec`).
  - `intel` — code intelligence for the edited language (highlighting, autocomplete, error squiggle).
  - `initialFiles` — the file(s) to open before any fetch/restore (so the editor is never empty).
  - `urls` — example files to fetch over HTTP at startup.
  - `title` / `tagline` — the header wordmark and subtitle.
  - `sessionKey` — the `localStorage` key the session is autosaved/restored under (host-unique).
  - `fileBrowser` — whether to show the activity bar and file pane. A single-file host (e.g. the CSS
    theme builder) sets this `False`, leaving just the code pane and the result.

-}
type alias Config pModel pMsg =
    { preview : Preview.Spec pModel pMsg
    , intel : CodeIntel
    , initialFiles : List ( String, String )
    , urls : List String
    , libUrls : List String
    , title : String
    , tagline : String
    , sessionKey : String
    , fileBrowser : Bool
    }


{-| Per-language code intelligence the shell drives the code pane with, supplied by the host:

  - `highlight` — source → `(class, text)` token segments for the syntax colouring.
  - `completions` — source + caret offset → autocomplete candidates for the word at the caret
    (return `[]` to disable autocomplete).
  - `accept` — source + caret + chosen candidate → the new (source, caret) with the word replaced.
  - `locate` — source + a preview error message → where to squiggle it (or `Nothing`).

The Elm playground wires Elm's `Highlight`/`Assist`; a CSS host wires the CSS highlighter and
no-op autocomplete/locate. -}
type alias CodeIntel =
    { highlight : String -> List ( String, String )
    , completions : String -> Int -> List String
    , accept : String -> Int -> String -> ( String, Int )
    , locate : String -> String -> Maybe { line : Int, column : Int, length : Int }
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


type Msg pMsg
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
    | PreviewMsg pMsg
    | Loaded String (Result Http.Error String)
    | LoadedLib String (Result Http.Error String)
    | Share
    | ShareInput String
    | Restore
    | GotHash String
    | LoadedSession (Maybe String)
    | GoBack
    | Scroll ScrollDir
    | DragStart Divider Float Float
    | DragMove Float Int
    | DragEnd
    | ToggleLeftPanel LeftPanel
    | NoOp


{-| Keyboard scrolling of the code pane: a page up/down, or a jump to the top/bottom of the file. -}
type ScrollDir
    = ScrollPageUp
    | ScrollPageDown
    | ScrollTop
    | ScrollBottom


{-| Builds an editor that fetches each example URL at startup and lets the user edit them. -}
program : Config pModel pMsg -> Program () (Model pModel pMsg) (Msg pMsg)
program config =
    Browser.element
        { init = \_ -> initApp config
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


{-| The initial editor: a single starter file, with the preview pane initialised from it, plus the
startup commands (restore a permalink/autosave, fetch the example URLs and any scripting libs). -}
initApp : Config pModel pMsg -> ( Model pModel pMsg, Cmd (Msg pMsg) )
initApp config =
    let
        files =
            config.initialFiles

        selected =
            Tuple.first (firstFile files)

        ( preview, previewCmd ) =
            config.preview.init { files = files, selected = selected, libs = [] }
    in
    ( { files = files
      , libs = []
      , selected = selected
      , preview = preview
      , config = config
      , newName = ""
      , caret = 0
      , completions = []
      , shareText = ""
      , restored = False
      , collapsed = Set.empty
      , sidebarWidth = Nothing
      , resultWidth = Nothing
      , drag = Nothing
      , leftPanel =
            if config.fileBrowser then
                Just FilesPanel

            else
                Nothing
      }
    , Cmd.batch
        [ Browser.Navigation.getHash GotHash
        , Storage.load config.sessionKey LoadedSession
        , fetchAll config.urls
        , fetchLibs config.libUrls
        , Cmd.map PreviewMsg previewCmd
        ]
    )


fetchLibs : List String -> Cmd (Msg pMsg)
fetchLibs urls =
    Cmd.batch (List.map (\url -> Http.get { url = url, expect = Http.expectString (LoadedLib (baseName url)) }) urls)


{-| Wires live effects: pane-divider dragging (whenever a drag is in progress) plus the selected
file's own subscriptions (a `game`'s loop, an app's keyboard/mouse, or a `Time.every` tick). -}
subscriptions : Model pModel pMsg -> Sub (Msg pMsg)
subscriptions model =
    Sub.batch
        [ dragSubscription model
        , Sub.map PreviewMsg (model.config.preview.subscriptions (contextOf model) model.preview)
        ]


{-| While a divider is held, follow the pointer (document-wide, so it keeps tracking past the bar)
and release on mouse-up. -}
dragSubscription : Model pModel pMsg -> Sub (Msg pMsg)
dragSubscription model =
    case model.drag of
        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove
                    (Decode.map2 DragMove
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "buttons" Decode.int)
                    )
                , Browser.Events.onMouseUp (Decode.succeed DragEnd)
                ]

        Nothing ->
            Sub.none


fetchAll : List String -> Cmd (Msg pMsg)
fetchAll urls =
    Cmd.batch (List.map (\url -> Http.get { url = url, expect = Http.expectString (Loaded url) }) urls)




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
groupedFiles : Model pModel pMsg -> List ( String, List ( String, String ) )
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
selectedFile : Model pModel pMsg -> List ( String, String )
selectedFile model =
    ( model.selected, lookup model.selected model.files |> Maybe.withDefault "" ) :: model.libs


{-| The shell's view of the sources, handed to a pluggable preview pane (see `Preview.Context`). This
is the seam the result column is being moved behind: today the interpreter preview is wired in
directly; it will consume this `Context` instead of reaching into the whole `Model`. -}
contextOf : Model pModel pMsg -> Preview.Context
contextOf model =
    { files = model.files, selected = model.selected, libs = model.libs }


{-| Recompute the preview pane from the current sources, after an edit or a file switch. -}
refreshPreview : Model pModel pMsg -> ( Model pModel pMsg, Cmd (Msg pMsg) )
refreshPreview model =
    let
        ( preview, cmd ) =
            model.config.preview.sourcesChanged (contextOf model) model.preview
    in
    ( { model | preview = preview }, Cmd.map PreviewMsg cmd )


{-| Re-initialises the running app from the selected file (its model becomes `init`) when that file
is a Browser.sandbox-style program; otherwise the app slot is unused. -}


update : Msg pMsg -> Model pModel pMsg -> ( Model pModel pMsg, Cmd (Msg pMsg) )
update msg model =
    case msg of
        SelectFile name ->
            refreshPreview { model | selected = name }

        EditAt content caret ->
            -- Recompute autocomplete candidates for the word the caret sits in, then re-run the file.
            withAutosave
                (refreshPreview
                    { model
                        | files = setFile model.selected content model.files
                        , caret = caret
                        , completions = model.config.intel.completions content caret
                    }
                )

        AcceptCompletion choice ->
            -- Replace the half-typed word with the chosen completion.
            let
                source =
                    lookup model.selected model.files |> Maybe.withDefault ""

                ( newSource, newCaret ) =
                    model.config.intel.accept source model.caret choice
            in
            refreshPreview
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
                    refreshPreview
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
                refreshPreview { model | files = model.files ++ [ ( name, "main = text \"new file\"" ) ], selected = name, newName = "" }

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
            refreshPreview { model | files = model.files ++ [ ( unique, content ) ], selected = unique }

        RemoveFile name ->
            let
                remaining =
                    List.filter (\f -> Tuple.first f /= name) model.files
            in
            refreshPreview
                { model
                    | files = remaining
                    , selected =
                        if model.selected == name then
                            remaining |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault ""

                        else
                            model.selected
                }

        PreviewMsg pMsg ->
            let
                ( preview, cmd ) =
                    model.config.preview.update (contextOf model) pMsg model.preview
            in
            ( { model | preview = preview }, Cmd.map PreviewMsg cmd )

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
                        refreshPreview { model | files = files }

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
                    refreshPreview { model | libs = libs }

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

        DragMove x buttons ->
            -- The pointer moved while dragging: resize the pane by its delta from the grab point. If
            -- no button is pressed (buttons == 0) the mouse was released without us seeing the mouseup
            -- (released over an iframe, or outside the window) — end the drag instead of following the
            -- cursor forever.
            case model.drag of
                Just d ->
                    if buttons == 0 then
                        ( { model | drag = Nothing }, Cmd.none )

                    else
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

        ToggleLeftPanel panel ->
            -- Clicking the active panel's activity-bar icon collapses the pane; otherwise show it.
            ( { model
                | leftPanel =
                    if model.leftPanel == Just panel then
                        Nothing

                    else
                        Just panel
              }
            , Cmd.none
            )

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


{-| Tacks an autosave of the model's files onto a command, so edits survive a page reload. -}
withAutosave : ( Model pModel pMsg, Cmd (Msg pMsg) ) -> ( Model pModel pMsg, Cmd (Msg pMsg) )
withAutosave ( m, cmd ) =
    ( m, Cmd.batch [ cmd, Storage.save m.config.sessionKey (Share.encodeFiles m.files) ] )


{-| Replaces the session with the files encoded in `text` (a permalink or autosaved string), marking
the session restored so the default examples (and a later autosave load) don't clobber it. -}
restoreSession : String -> Model pModel pMsg -> ( Model pModel pMsg, Cmd (Msg pMsg) )
restoreSession text model =
    case Share.decodeFiles text of
        [] ->
            ( model, Cmd.none )

        files ->
            refreshPreview
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


{-| The contents of the file named `name`, if present (a small assoc-list lookup, kept here so the
shell doesn't depend on the interpreter). -}
lookup : String -> List ( String, String ) -> Maybe String
lookup name files =
    case files of
        ( n, content ) :: rest ->
            if n == name then
                Just content

            else
                lookup name rest

        [] ->
            Nothing



-- VIEW


{-| A share/restore bar: "Share" encodes the whole session into the text box (copy it to share);
pasting a shared string and pressing "Restore" replaces the session with it. Pure (no ports). -}
shareBar : Model pModel pMsg -> Html (Msg pMsg)
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


view : Model pModel pMsg -> Html (Msg pMsg)
view model =
    div [ class "ed-root" ]
        [ div [ class "ed-header" ]
            [ backLink
            , span [ class "ed-title" ] [ text model.config.title ]
            , span [ class "ed-tagline" ] [ text model.config.tagline ]
            , shareBar model
            ]
        , div
            [ classList
                [ ( "ed-body", True )
                , ( "ed-dragging", model.drag /= Nothing )
                , ( "ed-left-collapsed", model.leftPanel == Nothing )
                ]
            ]
            (leftChrome model
                ++ [ codeColumn model
                   , divider ResultDivider
                   , resultColumn model
                   ]
            )
        ]


{-| The left-hand chrome — the activity bar, the file pane and its resize bar — when the host enables
the file browser, or nothing for a single-file host. The file pane and bar are always rendered (CSS
hides them when the panel is collapsed) so the result column keeps its DOM node: without that, a
preview holding externally-injected DOM (a vega-embed chart, a preview iframe) would be torn down and
re-created empty each time the file pane is toggled. -}
leftChrome : Model pModel pMsg -> List (Html (Msg pMsg))
leftChrome model =
    if model.config.fileBrowser then
        [ activityBar model, fileSidebar model, divider SidebarDivider ]

    else
        []


{-| The VSCode-style activity bar: a strip of icons on the far left choosing what fills the left pane.
Only the file list exists today; clicking the active icon collapses the pane. -}
activityBar : Model pModel pMsg -> Html (Msg pMsg)
activityBar model =
    div [ class "ed-activity" ]
        [ activityIcon model FilesPanel "Files" filesIcon ]


activityIcon : Model pModel pMsg -> LeftPanel -> String -> Html (Msg pMsg) -> Html (Msg pMsg)
activityIcon model panel label icon =
    button
        [ classList [ ( "ed-activity-btn", True ), ( "active", model.leftPanel == Just panel ) ]
        , title label
        , onClick (ToggleLeftPanel panel)
        ]
        [ icon ]


{-| A "files" glyph (a document with a folded corner and two lines) for the file-list activity icon. -}
filesIcon : Html msg
filesIcon =
    Svg.svg
        [ SvgA.viewBox "0 0 24 24"
        , SvgA.width "22"
        , SvgA.height "22"
        , SvgA.fill "none"
        , SvgA.stroke "currentColor"
        , SvgA.strokeWidth "1.6"
        , SvgA.strokeLinecap "round"
        , SvgA.strokeLinejoin "round"
        ]
        [ Svg.path [ SvgA.d "M14 3 H7 a2 2 0 0 0 -2 2 v14 a2 2 0 0 0 2 2 h10 a2 2 0 0 0 2 -2 V8 Z" ] []
        , Svg.path [ SvgA.d "M14 3 v5 h5" ] []
        , Svg.path [ SvgA.d "M9 13 h6 M9 16 h4" ] []
        ]


{-| A draggable bar between two panes. Grabbing it (mousedown) reads the adjacent pane's current pixel
width straight off the DOM event, so the following moves resize from a known starting point; the
default is prevented so the drag doesn't begin a text selection. -}
divider : Divider -> Html (Msg pMsg)
divider which =
    div
        [ class "ed-divider"
        , preventDefaultOn "mousedown" (dragStartDecoder which)
        ]
        []


{-| Decodes a divider mousedown into `DragStart`: the pointer x, plus the resized pane's width read
from the divider's sibling (the sidebar lies before its bar; the result column after its bar). -}
dragStartDecoder : Divider -> Decode.Decoder ( Msg pMsg, Bool )
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
widthStyle : Maybe Float -> List (Html.Attribute (Msg pMsg))
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
backLink : Html (Msg pMsg)
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


codeColumn : Model pModel pMsg -> Html (Msg pMsg)
codeColumn model =
    div [ class "ed-code-col", Html.Attributes.id codeColumnId ]
        [ div [ class "ed-code-card" ]
            [ div [ class "ed-filename" ] [ text model.selected ]
            , codeEditor model (lookup model.selected model.files |> Maybe.withDefault "")
            ]
        ]


{-| The right column (~40% of the width): the live result of the selected file's `main`, scrolling
on its own. -}
resultColumn : Model pModel pMsg -> Html (Msg pMsg)
resultColumn model =
    div (class "ed-result-col" :: widthStyle model.resultWidth)
        [ Html.map PreviewMsg (model.config.preview.view (contextOf model) model.preview) ]


{-| The syntax-highlighted code editor: a transparent `<textarea>` (which owns the caret, selection
and typing) layered over a `<pre>` of coloured `<span>`s. Both share the exact same font, padding and
wrapping, so the highlighted text underneath stays aligned with what's typed (the react-simple-code-
editor technique). The `<pre>` is in normal flow and sets the height; the textarea fills it. -}
codeEditor : Model pModel pMsg -> String -> Html (Msg pMsg)
codeEditor model source =
    div [ class "ed-editor" ]
        [ errorRibbon model source
        , div [ class "ed-editor-flex" ]
            [ gutter model source
            , div [ class "ed-code-area" ]
                [ pre [ class "code-text ed-pre" ]
                    (List.map renderSegment (model.config.intel.highlight source) ++ [ text "\n" ])
                , squiggleOverlay model source
                , textarea [ onEdit, onScrollKey, value source, class "code-text ed-textarea" ] []
                , completionBar model
                ]
            ]
        ]


{-| A line-number gutter beside the code, highlighting the line the caret is on. Aligned to the code
by sharing its font, size, line-height and top padding (long wrapped lines aside). -}
gutter : Model pModel pMsg -> String -> Html (Msg pMsg)
gutter model source =
    let
        lineCount =
            List.length (String.lines source)

        current =
            currentLine source model.caret
    in
    div [ class "ed-gutter" ]
        (List.map (gutterLine current) (List.range 1 lineCount))


gutterLine : Int -> Int -> Html (Msg pMsg)
gutterLine current n =
    div [ classList [ ( "ed-gutter-line", True ), ( "current", n == current ) ] ]
        [ text (String.fromInt n) ]


{-| The 1-based line the caret sits on (one past the newlines before it). -}
currentLine : String -> Int -> Int
currentLine source caret =
    1 + List.length (List.filter ((==) '\n') (String.toList (String.left caret source)))


{-| A `<textarea>` input handler that captures both the new text and the caret offset
(`selectionStart`), so autocomplete knows the word being typed. -}
onEdit : Html.Attribute (Msg pMsg)
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
onScrollKey : Html.Attribute (Msg pMsg)
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
scrollCode : ScrollDir -> Cmd (Msg pMsg)
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
completionBar : Model pModel pMsg -> Html (Msg pMsg)
completionBar model =
    if List.isEmpty model.completions then
        text ""

    else
        div [ class "ed-completion-bar" ]
            (List.map completionChip (List.take 12 model.completions))


completionChip : String -> Html (Msg pMsg)
completionChip label =
    button [ onMouseDown (AcceptCompletion label), class "ed-completion-chip" ]
        [ text label ]


{-| When the selected file fails to evaluate, surface the error — and, if it names an identifier that
appears in the source, the line/column where it is (computed by `Assist.squiggleFor`). -}
errorRibbon : Model pModel pMsg -> String -> Html (Msg pMsg)
errorRibbon model source =
    case model.config.preview.error model.preview of
        Just message ->
            div [ class "ed-error" ]
                [ text ("⚠ " ++ located model source ++ message) ]

        Nothing ->
            text ""


{-| A "line N, col C: " prefix locating the offending identifier in the source, or "" if none. -}
located : Model pModel pMsg -> String -> String
located model source =
    case squiggle model source of
        Just loc ->
            "line " ++ String.fromInt (loc.line + 1) ++ ", col " ++ String.fromInt (loc.column + 1) ++ ": "

        Nothing ->
            ""


{-| The location to squiggle: where the current error's offending identifier first appears, if any. -}
squiggle : Model pModel pMsg -> String -> Maybe { line : Int, column : Int, length : Int }
squiggle model source =
    case model.config.preview.error model.preview of
        Just message ->
            model.config.intel.locate source message

        Nothing ->
            Nothing


{-| The character offset of a 0-based line/column in `source` (newlines count as one char). -}
offsetOf : Int -> Int -> String -> Int
offsetOf line column source =
    let
        before =
            String.lines source |> List.take line |> List.map (\l -> String.length l + 1) |> List.sum
    in
    before + column


{-| An overlay aligned exactly over the highlight `<pre>` (same font/padding/wrapping) that draws a
wavy red underline under the offending identifier: the text is transparent, so the syntax-highlighted
code shows through, but the underline's own colour marks the error in place. -}
squiggleOverlay : Model pModel pMsg -> String -> Html (Msg pMsg)
squiggleOverlay model source =
    case squiggle model source of
        Nothing ->
            text ""

        Just loc ->
            let
                start =
                    offsetOf loc.line loc.column source

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


renderSegment : ( String, String ) -> Html (Msg pMsg)
renderSegment ( cls, txt ) =
    span [ class (segClass cls) ] [ text txt ]


{-| The CSS class for each highlighter token kind (`""` is the default foreground). -}
segClass : String -> String
segClass cls =
    if cls == "" then
        "seg"

    else
        "seg-" ++ cls


fileSidebar : Model pModel pMsg -> Html (Msg pMsg)
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
fileGroup : Model pModel pMsg -> ( String, List ( String, String ) ) -> Html (Msg pMsg)
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


fileRow : String -> ( String, String ) -> Html (Msg pMsg)
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


