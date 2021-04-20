module Email exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type Email = Email String

toString : Email -> String
toString (Email email) = email

encode : Email -> Value
encode (Email email) = Encode.string email

decoder : Decoder Email
decoder = Decode.map Email Decode.string
