module User.Asset exposing (..)

import Asset.Image exposing (Image, image)

defaultAvatar : Image
defaultAvatar =
    image "default-avatar.jpg"