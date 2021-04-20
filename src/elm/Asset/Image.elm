module Asset.Image exposing ( Image
                            , loading
                            , src
                            , image
                            )

import Html exposing (Attribute)
import Html.Attributes as Attr

type Image = Image String

image : String -> Image
image filename =
    "/assets/images/"
        |> String.append filename
        |> Image

loading : Image
loading =
    image "loading.svg"

src : Image -> Attribute msg
src (Image url) = Attr.src url