port module Api exposing ( Credential
                         , application
                         , login
                         , logout
                         , register
                         , decode
                         , username
                         , userChanges
                         , get
                         , post
                         , put
                         , patch
                         , delete
                         , storeCredWith
                         )

import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing ( Decoder
                                      , Value
                                      )
import Json.Decode.Pipeline exposing (required)
import User.Username as Username exposing (Username)
import Api.Endpoint as Endpoint exposing (Endpoint(..))
import Util.Request as Request
import User.Avatar exposing (Avatar)



login : Http.Body -> Decoder (Credential -> a) -> (Result Http.Error a -> msg) -> Cmd msg
login body decoder toMsg =
    post Endpoint.login Nothing body toMsg (Decode.field "user" (decoderFromCred decoder))

register : Http.Body -> Decoder (Credential -> a) -> (Result Http.Error a -> msg) -> Cmd msg
register _ _ = 
    Debug.todo "Endpoint to Register a new admin"

logout : Cmd msg
logout =
    storeCache Nothing

port onStorageChange : (Value -> msg) -> Sub msg
port storeCache : Maybe Value -> Cmd msg


type alias Token = String
type Credential = Credential Username Token

-- cacheStorageKey : String
-- cacheStorageKey =
--     "cache"

-- credStorageKey : String
-- credStorageKey =
--     "credential"


username : Credential -> Username
username (Credential val _) =
    val

credHeader : Credential -> Http.Header
credHeader (Credential _ token) =
    "Token " ++ token
        |> Http.header "authorization"


decode : Decoder (Credential -> user) -> Value -> Result Decode.Error user
decode decoder value =
    Decode.decodeValue Decode.string value
        |> Result.andThen
            (\str ->
                Decode.decodeString (Decode.field "user" (decoderFromCred decoder)) str)

decoderFromCred : Decoder (Credential -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credentialDecoder

credentialDecoder : Decoder Credential
credentialDecoder = 
    Decode.succeed Credential
        |> required "username" Username.decoder
        |> required "token"    Decode.string

userChanges : (Maybe user -> msg) -> Decoder (Credential -> user) -> Sub msg
userChanges toMsg decoder =
    onStorageChange (\value -> toMsg <| decodeFromChange decoder value)

decodeFromChange : Decoder (Credential -> user) -> Value -> Maybe user
decodeFromChange userDecoder val =
    Decode.decodeValue (storageDecoder userDecoder) val
        |> Result.toMaybe

storageDecoder : Decoder (Credential -> user) -> Decoder user
storageDecoder userDecoder =
    Decode.field "user" (decoderFromCred userDecoder)

-- Http Functions

get : Endpoint -> Maybe Credential -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
get (Endpoint u) maybeCred toMsg decoder =
    let
        reqHeader = case maybeCred of
            Nothing -> []

            Just cred -> [credHeader cred]
        
        expectation = Http.expectJson toMsg decoder

        requestConfig =
            Request.config expectation
                |> Request.method "GET"
                |> Request.url u
                |> Request.headers reqHeader
    in
        Request.request requestConfig

post : Endpoint -> Maybe Credential -> Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
post (Endpoint addrs) maybeCred b toMsg decoder =
    let
        reqHeader = case maybeCred of
            Nothing   -> []

            Just cred -> [credHeader cred]
        
        expectation = Http.expectJson toMsg decoder

        requestConfig = 
            Request.config expectation
                |> Request.method "POST"
                |> Request.url addrs
                |> Request.headers reqHeader
                |> Request.body b
    in
        Request.request requestConfig
    

put : Endpoint -> Credential -> Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
put (Endpoint addrs) cred b toMsg decoder =
    let
        expectation = Http.expectJson toMsg decoder

        requestConfig =
            Request.config expectation
                |> Request.method "PUT"
                |> Request.url addrs
                |> Request.headers [credHeader cred]
                |> Request.body b
    in
        Request.request requestConfig


delete : Endpoint -> Credential -> Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
delete (Endpoint addrs) cred b toMsg decoder =
    let
        expectation = Http.expectJson toMsg decoder

        requestConfig =
            Request.config expectation
                |> Request.method "DELETE"
                |> Request.url addrs
                |> Request.headers [credHeader cred]
                |> Request.body b
    in
        Request.request requestConfig


patch : Endpoint -> Credential -> Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
patch (Endpoint addrs) cred b toMsg decoder =
    let
        expectation = Http.expectJson toMsg decoder
        
        requestConfig =
            Request.config expectation
            |> Request.method "PATCH"
            |> Request.url addrs
            |> Request.headers [credHeader cred]
            |> Request.body b
    in
        Request.request requestConfig



storeCredWith : Credential -> Avatar -> Cmd msg
storeCredWith (Credential uname token) avatar =
    let
        json =
            Encode.object
                [ ("user"
                  , Encode.object
                        [ ("username", Username.encode uname)
                        , ("token"   , Encode.string token)
                        , ("image"   , User.Avatar.encode avatar )
                        ]
                  )
                ]
    in
        storeCache (Just json)


application :
    Decoder (Credential -> user)
    ->  { init : Maybe user -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application viewerDecoder config =
    let
        init flags url navKey =
            let
                maybeUser =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (storageDecoder viewerDecoder))
                        |> Result.toMaybe
            in
            config.init maybeUser url navKey
    in
        Browser.application
            { init = init
            , onUrlChange = config.onUrlChange
            , onUrlRequest = config.onUrlRequest
            , subscriptions = config.subscriptions
            , update = config.update
            , view = config.view
            }
