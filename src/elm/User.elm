module User exposing (..)

import User.Avatar as Avatar exposing (Avatar)
import Api exposing (Credential)
import User.Username exposing (Username)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom)


type User = User Avatar Credential

cred : User -> Credential
cred (User _ val) = val

username : User -> Username
username (User _ val) = Api.username val

avatar : User -> Avatar
avatar (User val _) = val

minPasswordChars : Int
minPasswordChars = 6

decoder : Decoder (Credential -> User)
decoder =
    Decode.succeed User
        |> custom (Decode.field "image" Avatar.decoder)

