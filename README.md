# elm-editor — an Elm interpreter written in Elm

An Ellie-style, in-browser code editor whose evaluator is **a from-scratch Elm interpreter written
in Elm itself**. It lexes, parses and evaluates a broad teaching subset of Elm live in the browser:
pure values, `Html`/TEA apps (`Browser.sandbox`/`element`), inline SVG, a built-in `elm-playground`,
and live effects (`Random`, `Time`, `Http`, `File`). It has syntax highlighting, autocomplete, a
shareable-permalink encoder and a time-travel debugger.

Written for the [elm-lang](https://github.com/tunguski/elm-lang) implementation of Elm and showcased
in its [example gallery](https://tunguski.github.io/elm-lang/editor.html).

> Canonical home: <https://github.com/tunguski/elm-editor>.

## Layout

All modules are top-level (in `src/`):

| Module | Role |
|---|---|
| `Lang` | Core types: `Value`, `Expr`, `Pattern`, `Decl`, `Env`, `Globals`. |
| `Lexer` | Tokeniser (+ layout/offside cooking). |
| `Parser` | Recursive-descent parser → `Expr`/`Decl` (single module and multi-file project). |
| `Eval` | The evaluator: `eval`, `evalProject`, TEA app driver, effects, playground/game, WebGL bridge. |
| `EvalJson` / `EvalPlayground` / `EvalRender` | JSON codecs, the `elm-playground` runtime, and HTML/SVG rendering for `Eval`. |
| `Highlight` | Token-based syntax highlighting. |
| `Assist` | Autocomplete, word-at-cursor, error squiggles. |
| `Share` | Encode/decode the file set into a shareable permalink. |
| `CodeEditor` | A standalone, language-agnostic syntax-highlighted editing widget (a transparent textarea over a highlighted `<pre>`); the embedder owns state. |
| `Preview` | The contract between the shell and a pluggable result pane: `Context` (the current sources) + `Spec` (`init`/`sourcesChanged`/`update`/`subscriptions`/`view`/`error`). No interpreter dependency. |
| `ElmPreview` | The elm-lang result pane: interprets and runs the selected file's `main` (live TEA app, `elm-playground` game loop, time-travel debugger, effects). The **only** module that imports `Eval`. |
| `Editor` | The abstract, embeddable shell: file browser, code pane, layout/resize, autocomplete, sharing — generic over a pluggable preview (`Preview.Spec`), with no dependency on the interpreter. |
| `Main` | The elm-lang instance: `Editor.program { preview = ElmPreview.spec, urls = … }`. |

## Embedding the editor with a custom pane

The shell is **abstract**: it owns the IDE chrome (file browser, code editing, layout/resize,
sharing, persistence) and renders whatever *preview pane* you plug in. The interpreter is not part of
the shell — it lives entirely behind one preview (`ElmPreview`). To host the editor elsewhere (for
example a [Bootstrap theme builder](https://github.com/tunguski) that edits CSS and renders a live
theme), supply your own `Preview.Spec`:

```elm
myPreview : Preview.Spec MyModel MyMsg
myPreview =
    { init           = \ctx -> ...          -- build the pane from the starting sources
    , sourcesChanged = \ctx model -> ...     -- recompute when a file is edited / switched
    , update         = \ctx msg model -> ... -- fold a pane message
    , subscriptions  = \ctx model -> ...
    , view           = \ctx model -> ...     -- render the result column (Html MyMsg)
    , error          = \model -> ...         -- current error, surfaced as a code-pane squiggle
    }

main =
    Editor.program { preview = myPreview, urls = [ "theme.css" ] }
```

`Preview.Context` is `{ files, selected, libs }` — the current sources the shell hands you. The shell
wraps your `MyMsg` as `Editor.Msg`, so the chrome and the pane never see each other's messages.

The reusable building blocks (the "hybrid" of a drop-in program plus pieces):

- **`Editor.program : Config pModel pMsg -> Program …`** — the batteries-included shell; you write
  only the preview.
- **`CodeEditor`** — a standalone, language-agnostic editing widget (pass it a `highlight` function —
  `Highlight.segments` for Elm, `Highlight.cssSegments` for CSS — and an `onChange`); usable on its
  own, outside `Editor`.
- **`editor.css`** — the shell's stylesheet (the `ed-*` and `ce-*` classes), shipped here so any
  embedder is styled; link it and override the classes to re-theme.

## Build & run

You need the [elm-lang](https://github.com/tunguski/elm-lang) CLI. Point `ELM` at it (its `elm.sh`
wrapper, `java -jar elm.jar`, or the native binary).

```sh
elm make src/Main.elm --project=elm.json -o build/editor.html --no-check
# then open build/editor.html in a browser
```

`--no-check` is used because this is a large program that leans on idioms the elm-lang type checker
doesn't fully analyse; the gallery compiles it the same way. It runs correctly under the interpreter
and the JS backend (both are differentially tested in elm-lang).

### Dependencies

Package dependencies (`elm/browser`, `elm/html`, `elm/file`, `elm/http`, `elm/time`,
`elm-explorations/webgl`, `elm/core`) are in [elm.json](elm.json), added with the elm-lang package
manager (`elm install <pkg> --elm`). The editor also imports **`Storage`** (browser `localStorage`
save/load), which is a built-in module provided by the elm-lang runtime — it is not a package and so
does not appear in `elm.json`.
