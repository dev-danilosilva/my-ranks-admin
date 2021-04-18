module User.Username exposing ( Username
                              , decoder
                              , encode
                              , toString
                              , urlParser
                              , toHtml
                              )

import Html exposing (Html)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Url.Parser as UrlParser


type Username = Username String

decoder : Decoder Username
decoder =
    Decode.map Username Decode.string

encode : Username -> Value
encode (Username username) =
    Encode.string username

toString : Username -> String
toString (Username username) = username

urlParser : UrlParser.Parser (Username -> a) a
urlParser =
    UrlParser.custom "USERNAME" (\str -> Just (Username str))

toHtml : Username -> Html msg
toHtml (Username username) =
    Html.text username
