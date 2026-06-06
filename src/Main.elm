module Main exposing (main)

{-| The hosted Elm playground instance of the reusable {@link Editor} shell. It supplies the
Elm-specific configuration — the interpreter preview (`ElmPreview`), Elm code intelligence
(`Highlight`/`Assist`), a starter file and the example URLs — and the shell provides the generic IDE
chrome (file browser, source editing, layout/resize, sharing, persistence). A different host (the CSS
theme builder, the Vega editor) supplies a different `Config` to the same shell.
-}

import Assist
import Editor
import ElmPreview
import Highlight


main : Program () (Editor.Model ElmPreview.Model ElmPreview.Msg) (Editor.Msg ElmPreview.Msg)
main =
    Editor.program
        { preview = ElmPreview.spec
        , intel = elmIntel
        , initialFiles = [ starter ]
        , urls = exampleUrls
        , title = "Elm-in-Elm playground"
        , tagline = "files · code · live result"
        , sessionKey = "elm-editor-session"
        }


{-| Elm code intelligence wired into the shell's code pane: syntax highlighting, identifier
autocomplete (the word at the caret) and locating an interpreter error back in the source. -}
elmIntel : Editor.CodeIntel
elmIntel =
    { highlight = Highlight.segments
    , completions = \source caret -> Assist.completions source (Assist.wordAt source caret)
    , accept = Assist.accept
    , locate = \source message -> Maybe.andThen (Assist.squiggleFor source) (Assist.errorName message)
    }


{-| A built-in starter file so the editor is usable immediately (and offline / before fetches). -}
starter : ( String, String )
starter =
    ( "Buttons.elm"
    , "module Main exposing (main)\n\nimport Browser\nimport Html exposing (button, div, text)\nimport Html.Events exposing (onClick)\n\nmain = Browser.sandbox { init = init, update = update, view = view }\n\ninit = 0\n\nupdate msg model =\n    case msg of\n        Increment ->\n            model + 1\n\n        Decrement ->\n            model - 1\n\nview model =\n    div []\n        [ button [ onClick Decrement ] [ text \"-\" ]\n        , div [] [ text (String.fromInt model) ]\n        , button [ onClick Increment ] [ text \"+\" ]\n        ]\n"
    )


{-| The example files the editor loads at startup, served by the gallery under `examples/`: the full
elm-lang.org gallery examples plus TodoMVC — editable and viewable here, though many use features
(SVG, WebGL, HTTP, …) the small in-browser interpreter can't run, so their result pane shows what it
can. -}
exampleUrls : List String
exampleUrls =
    [ "examples/TodoMvc.elm"
    ]
        ++ List.map (\slug -> "examples/" ++ slug ++ ".elm")
            [ "Hello", "Groceries", "Shapes", "Buttons", "TextFields", "Forms", "Numbers", "Cards"
            , "Positions", "Book", "Quotes", "Time", "Clock", "Upload", "DragAndDrop"
            , "ImagePreviews", "Triangle", "Cube", "Crate", "Thwomp", "FirstPerson", "Picture"
            , "Animation", "Mouse", "Keyboard", "Turtle", "Mario", "Life"
            ]
