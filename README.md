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
| `Editor` | The `Browser.element` UI: file list, code pane, output, debugger. |
| `Main` | Entry point (`Editor.program`). |

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
