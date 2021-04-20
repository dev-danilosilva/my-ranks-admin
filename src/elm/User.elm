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
username (User _ u) = Api.username u

avatar : User -> Avatar
avatar (User a _) = a

minPasswordChars : Int
minPasswordChars = 6

decoder : Decoder (Credential -> User)
decoder =
    Decode.succeed User
        |> custom (Decode.field "image" Avatar.decoder)

store : User -> Cmd msg
store (User _ _) =
    Debug.todo "Store Crendentials in Local Storage"