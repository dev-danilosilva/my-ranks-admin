module Page exposing ( Page(..)
                     , view
                     )

import User exposing (User)
import Browser exposing (Document)
import Html exposing (Html, text, div, a)
import Html.Attributes exposing (href)

type Page
    = Login
    | Dashboard
    | Other


view : Maybe User -> Page -> { title : String, content : Html msg } -> Document msg
view maybeUser page { title, content } =
    { title = title
    , body = [ viewHeader page maybeUser
             , content
             ]
    }

viewHeader : Page -> Maybe User -> Html msg
viewHeader _ _ =
    div []
        [ text "Page Header"
        , a [href "http://www.google.com"] [text "Google"]
        ]
    