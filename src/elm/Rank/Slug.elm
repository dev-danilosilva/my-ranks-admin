module Rank.Slug exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Url.Parser exposing (Parser)

type Slug = Slug String

toString : Slug -> String
toString (Slug str) = str

decoder : Decoder Slug
decoder =
    Decode.map Slug Decode.string

urlParser : Parser (Slug -> a) a
urlParser =
    Url.Parser.custom "SLUG" (\str -> Just (Slug str))
