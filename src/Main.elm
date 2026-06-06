module Main exposing (main)

{-| The hosted instance of the reusable {@link Editor}: it only names the example files to offer.
The editor fetches each over HTTP at startup (relative to the page) and presents them as editable
files; the editor shell (file browser, source editing, live rendering of each file's `main`) lives
in the `Editor` module, so it can be embedded elsewhere with a different list. -}

import Editor


main : Program () Editor.Model Editor.Msg
main =
    Editor.program exampleUrls


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
