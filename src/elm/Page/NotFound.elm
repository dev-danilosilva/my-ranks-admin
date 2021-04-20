module Page.NotFound exposing (..)

import Html exposing (Html, div, text)

view : model -> {title : String, content : Html msg}
view _ =
    { title = "Not Found"
    , content =
        div []
            [text "Page Not Found"]
    }
    

