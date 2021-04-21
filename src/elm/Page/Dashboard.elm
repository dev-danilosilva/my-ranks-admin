module Page.Dashboard exposing ( Model
                               , Msg(..)
                               , init
                               )


import Session exposing (Session)


type alias Model =
    { session : Session
    , errors : List String
    }

type Msg
    = GotSession Session

init : Session -> (Model, Cmd Msg)
init session =
    ( { session = session
      , errors = []
      }
    , Cmd.none
    )
