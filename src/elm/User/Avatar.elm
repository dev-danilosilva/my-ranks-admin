module User.Avatar exposing ( Avatar
                            , src
                            , encode
                            , decoder
                            , toMaybeString
                            )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Html exposing (Attribute)
import Asset.Image as UserAsset
import Html.Attributes


type Avatar = Avatar (Maybe String)


decoder : Decoder Avatar
decoder =
    Decode.map Avatar (Decode.nullable Decode.string)


encode : Avatar -> Value
encode (Avatar maybeUrl) =
    case maybeUrl of
        Just url ->
            Encode.string url
        
        Nothing ->
            Encode.null


src : Avatar -> Attribute msg
src (Avatar maybeUrl) =
    case maybeUrl of
        Just ""  ->
            UserAsset.src UserAsset.defaultAvatar
        
        Just url ->
            Html.Attributes.src url

        Nothing ->
            UserAsset.src UserAsset.defaultAvatar
            

toMaybeString : Avatar -> Maybe String
toMaybeString (Avatar maybeUrl) =
    maybeUrl