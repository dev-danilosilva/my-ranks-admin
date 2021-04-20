module Page.Dashboard exposing (Model)


import Session exposing (Session)


type alias Model =
    { session : Session
    , errors : List String
    }

