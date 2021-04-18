port module Api exposing (..)


import Http
import Json.Decode as Decode exposing ( Decoder
                                      , Value
                                      )
import Json.Decode.Pipeline exposing (required)
import User.Username as Username exposing (Username)
import Api.Endpoint as Endpoint exposing (Endpoint(..))
import Util.Request exposing ( request
                             , configRequest
                             , method
                             , body
                             , headers
                             , url
                             )

port onStorageChange : (Value -> msg) -> Sub msg
port storeCache : Maybe Value -> Cmd msg


type alias Token = String
type Credential = Credential Username Token

cacheStorageKey : String
cacheStorageKey =
    "cache"

credStorageKey : String
credStorageKey =
    "credential"

logout : Cmd msg
logout =
    storeCache Nothing

username : Credential -> Username
username (Credential val _) =
    val

credHeader : Credential -> Http.Header
credHeader (Credential _ token) =
    Http.header "authorization" 
        <| "Token " ++ token


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
    in
        configRequest expectation
            |> method "GET"
            |> url u
            |> headers reqHeader
            |> request

post : Endpoint -> Maybe Credential -> Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
post (Endpoint addrs) maybeCred b toMsg decoder =
    let
        reqHeader = case maybeCred of
            Nothing   -> []

            Just cred -> [credHeader cred]
        
        expectation = Http.expectJson toMsg decoder
    in
        configRequest expectation
            |> method "POST"
            |> url addrs
            |> headers reqHeader
            |> body b
            |> request
    

put : Endpoint -> Credential -> Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
put (Endpoint addrs) cred b toMsg decoder =
    let
        expectation = Http.expectJson toMsg decoder
    in
        configRequest expectation
            |> method "PUT"
            |> url addrs
            |> headers [credHeader cred]
            |> body b
            |> request


delete : Endpoint -> Credential -> Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
delete (Endpoint addrs) cred b toMsg decoder =
    let
        expectation = Http.expectJson toMsg decoder
    in
        configRequest expectation
            |> method "DELETE"
            |> url addrs
            |> headers [credHeader cred]
            |> body b
            |> request


patch : Endpoint -> Credential -> Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
patch (Endpoint addrs) cred b toMsg decoder =
    let
        expectation = Http.expectJson toMsg decoder
    in
        configRequest expectation
            |> method "PATCH"
            |> url addrs
            |> headers [credHeader cred]
            |> body b
            |> request


login : Http.Body -> Decoder (Credential -> a) -> (Result Http.Error a -> msg) -> Cmd msg
login b decoder toMsg =
    post Endpoint.login Nothing b toMsg (Decode.field "user" (decoderFromCred decoder))







