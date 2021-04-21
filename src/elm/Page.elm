module Page exposing ( Page(..)
                     , view
                     )

import User exposing (User)
import Browser exposing (Document)
import Html exposing (Html, text, header, div)
import Html.Attributes exposing (class)

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
    header [class "standard-header"]
        [ div [class "logo"] [text "My Ranks"]
        , div [class "menu-items"] []
        ]
    