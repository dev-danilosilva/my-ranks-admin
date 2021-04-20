module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Session exposing (Session)
import User exposing (User)
import Url exposing (Url)
import Route exposing (Route)

import Html exposing (text)



type Model
    = Redirect Session
    | NotFound Session
    -- | Login Login.Model
    -- | Profile Username Profile.Model
    -- | Editor (Maybe Slug) Editor.Model

type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest

init : Maybe User -> Url -> Nav.Key -> (Model, Cmd Msg)
init maybeUser url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromCurrentUser navKey maybeUser))

changeRouteTo : Maybe Route -> Model -> (Model, Cmd Msg)
changeRouteTo maybeRoute model =
    let
        session = toSession model
    in
        case maybeRoute of
            Nothing ->
                (NotFound session, Cmd.none)
            
            Just _ -> (model, Cmd.none)


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session
        
        NotFound session ->
            session


main : Html.Html msg
main = text "My Ranks Admin"
