module Loading exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (width, height, alt)
import Asset.Image as Asset

icon : Html msg
icon =
    Html.img
        [ Asset.src Asset.loading
        , width 64
        , height 64
        , alt "Loading..."
        ]
        []

error : String -> Html msg
error str =
    Html.text ("Error loading " ++ str ++ ".")

