module Route exposing ( Route(..)
                      , href
                      , replaceUrl
                      , fromUrl
                      )

import Browser.Navigation as Nav
import User.Username exposing (Username)
import Url.Parser as UrlParser exposing ( Parser
                                        , (</>)
                                        )

import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)


type Route
    = Login
    | Logout
    | Dashboard


parser : Parser (Route -> a) a
parser =
    UrlParser.oneOf
        [ UrlParser.map Login  (UrlParser.s "login")
        , UrlParser.map Logout (UrlParser.s "logout")
        , UrlParser.map Dashboard (UrlParser.s "dashboard")
        ]


-- Global Helpers

href : Route -> Attribute msg
href targetRoute = Attr.href
                        <| toString targetRoute

replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key
        <| toString route

fromUrl : Url -> Maybe Route
fromUrl url =
    { url
        | path = Maybe.withDefault "" url.fragment
        , fragment = Nothing
    } |> UrlParser.parse parser



-- Internals

toString : Route -> String
toString route =
    "#/" ++ String.join "/" (toPieces route)

toPieces : Route -> List String
toPieces route =
    case route of
        Login ->
            ["login"]
        
        Logout ->
            ["logout"]
        
        Dashboard ->
            ["dashboard"]