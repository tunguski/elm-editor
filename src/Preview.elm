module Preview exposing (Context, Spec)

{-| The contract between the abstract `Editor` shell and a pluggable **preview pane**.

The editor owns the generic IDE chrome — the file browser, tabs, code editing, layout/resize,
scrolling, sharing and persistence. What goes in the result column is the embedder's: elm-lang plugs
in an "interpret & run the selected file's `main`" preview (`ElmPreview`); a Bootstrap theme builder
plugs in a "render the theme from the edited CSS" preview. A preview is a small TEA the shell drives:
its model and message types are the embedder's (`pModel` / `pMsg`), and the shell threads them
through opaquely.

This module holds only the contract types (no logic, no dependency on the interpreter), so both the
shell and any preview can import it without a cycle.

@docs Context, Spec

-}

import Html exposing (Html)


{-| What the shell hands a preview: the current editable files (filename/contents), which one is
selected, and any hidden library modules merged into scope. A preview recomputes itself from this
whenever the user edits or switches files (`sourcesChanged`).
-}
type alias Context =
    { files : List ( String, String )
    , selected : String
    , libs : List ( String, String )
    }


{-| A pluggable preview pane, as a record of functions over the embedder's own `pModel`/`pMsg`:

  - `init` — build the initial preview state from the starting `Context`.
  - `sourcesChanged` — recompute when the selected file or its contents change (the shell calls this
    on every edit and file switch).
  - `update` — fold a preview message into the preview state (e.g. an interpreted app's `Msg`).
  - `subscriptions` — the preview's live subscriptions (a game loop, a `Time.every` tick, …).
  - `view` — render the result column.

The shell wraps `pMsg` in its own message type and `Html.map`s the view, so a preview never sees the
editor's chrome messages and vice versa.
-}
type alias Spec pModel pMsg =
    { init : Context -> ( pModel, Cmd pMsg )
    , sourcesChanged : Context -> pModel -> ( pModel, Cmd pMsg )
    , update : pMsg -> pModel -> ( pModel, Cmd pMsg )
    , subscriptions : pModel -> Sub pMsg
    , view : pModel -> Html pMsg
    }
