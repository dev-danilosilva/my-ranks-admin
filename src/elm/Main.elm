module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Session exposing (Session)
import User exposing (User)
import Url exposing (Url)
import Route exposing (Route)

import Page.Login as LoginPage
import Api
import Json.Decode exposing (Value)
import Html exposing (text)


type Model
    = Redirect Session
    | NotFound Session
    | Login LoginPage.Model
    -- | Dashboard Dashboard.Model

type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotLoginMsg LoginPage.Msg
    | GotSession Session

init : Maybe User -> Url -> Nav.Key -> (Model, Cmd Msg)
init maybeUser url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromCurrentUser navKey maybeUser))

subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none
        
        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))
        
        Login login ->
            Sub.map GotLoginMsg (LoginPage.subscriptions login)


changeRouteTo : Maybe Route -> Model -> (Model, Cmd Msg)
changeRouteTo maybeRoute model =
    let
        session = toSession model
    in
        case maybeRoute of
            Nothing ->
                (NotFound session, Cmd.none)
            
            Just Route.Login ->
                LoginPage.init session
                    |> updateWith Login GotLoginMsg
            
            Just _ -> (model, Cmd.none)

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> (subModel, Cmd subMsg) -> (Model, Cmd Msg)
updateWith toModel toMsg (subModel, subCmd) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
    

toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session
        
        NotFound session ->
            session
        
        Login loginModel ->
            LoginPage.toSession loginModel


main : Program Value Model Msg
main =
    Api.application User.decoder
        { init = init
        , onUrlChange   = ChangedUrl
        , onUrlRequest  = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        _ -> (model, Cmd.none)
    


view : Model -> Browser.Document Msg
view _ = 
    { title = "App"
    , body = [text "Hello World"]
    }