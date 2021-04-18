module Asset.Image exposing ( Image
                                 , error
                                 , loading
                                 , defaultAvatar
                                 , src
                                 )

import Html exposing (Attribute)
import Html.Attributes as Attr

type Image = Image String

error : Image
error =
    image "error.jpg"

loading : Image
loading =
    image "loading.svg"

defaultAvatar : Image
defaultAvatar =
    image "default-avatar.jpg"

image : String -> Image
image filename =
    "/assets/images/"
        |> String.append filename
        |> Image

src : Image -> Attribute msg
src (Image url) = Attr.src url