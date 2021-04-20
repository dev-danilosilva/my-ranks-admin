module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Session exposing (Session)
import User exposing (User)
import Url exposing (Url)
import Route exposing (Route)

import Page
import Page.Login as LoginPage
import Api
import Json.Decode exposing (Value)
import Html
import Html.Attributes


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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model) of
        (ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            (model, Cmd.none)
                        
                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )
        
        (ChangedUrl url, _) ->
            changeRouteTo (Route.fromUrl url) model
        

        (GotLoginMsg subMsg, Login subModel) ->
            LoginPage.update subMsg subModel
                |> updateWith Login GotLoginMsg

        (GotSession session, Redirect _) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Dashboard)

        _ -> (model, Cmd.none)
    


view : Model -> Browser.Document Msg
view model = 
    let
        user =
            Session.currentUser (toSession model)
        
        viewPage page toMsg config =
            let
                {title, body} = Page.view user page config
            in
                { title = title
                , body = List.map (Html.map toMsg) body
                }
            
    in
        case model of
            Login subModel ->
                viewPage Page.Login GotLoginMsg (LoginPage.view subModel)
            
            _ -> 
                { title = "Unknown", body = [Html.text "Unknown Page", Html.a [Html.Attributes.href "#/login"] [Html.text "login"]]}
    

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
            
            Just _ ->
                LoginPage.init session
                    |> updateWith Login GotLoginMsg

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> (subModel, Cmd subMsg) -> (Model, Cmd Msg)
updateWith toModel toMsg (subModel, subCmd) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
    

toSession : Model -> Session
toSession model =
    case model of
        Redirect session ->
            session
        
        NotFound session ->
            session
        
        Login loginModel ->
            LoginPage.toSession loginModel
