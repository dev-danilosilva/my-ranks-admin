module Page.Login exposing ( Model
                           , Msg(..)
                           , validate
                           , validateField
                           , login
                           , Problem(..)
                           , ValidatedField(..)
                           , toSession
                           , init
                           , subscriptions
                           , update
                           , view
                           )

import Api
import Json.Encode as Encode
import Session exposing (Session)
import User exposing (User)
import Http
import Route
import Html as H exposing (Html)
import Html.Events as HEvents
import Html.Attributes as Attr

type alias Model =
    { session : Session
    , problem : List Problem
    , form : Form
    }

type alias Form =
    { email : String
    , password : String
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String

type ValidatedField
    = Email
    | Password

type TrimmedForm = TrimmedForm Form

type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (Result Http.Error User)
    | GotSession Session

init : Session -> (Model, Cmd msg)
init session =
    ( { session = session
      , problem = []
      , form =
            { email = ""
            , password = ""
            }
      }, Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CompletedLogin (Ok user) ->
            ( model
            , User.store user
            )
        
        EnteredEmail email ->
            updateModelForm (\form -> { form | email = email }) model
                |> Tuple.pair
                |> (\f -> f Cmd.none)
        
        EnteredPassword password ->
            updateModelForm (\form -> { form | password = password }) model
                |> Tuple.pair
                |> (\f -> f Cmd.none)
        
        GotSession session ->
            ( {model | session = session}
            , Route.replaceUrl (Session.navKey session) Route.Dashboard
            )
        
        _ -> (model, Cmd.none)


view : Model -> { title : String, content : Html Msg }
view _ =
    { title = "Login"
    , content =
        H.div [Attr.class "container login-page"]
            [ H.div [Attr.class "login-form"]
                [ H.input [ HEvents.onInput EnteredEmail
                          , Attr.type_ "email"
                          , Attr.class "email-field"
                          , Attr.placeholder "Email"
                          ]
                          []
                , H.input [ HEvents.onInput EnteredPassword
                          , Attr.type_ "password"
                          , Attr.class "password-field"
                          , Attr.placeholder "Password"
                          ]
                          []
                , H.span [ HEvents.onClick SubmittedForm
                         , Attr.class "login-button"
                         ]
                            [ H.text "Login" ]
                ]
            ]
    }

updateModelForm : (Form -> Form) -> Model -> Model
updateModelForm f model =
    { model | form = f model.form }

-- Form

fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
    , Password
    ]

trimFields : Form -> TrimmedForm
trimFields form =
    TrimmedForm
        { email = String.trim form.email
        , password = String.trim form.password
        }

validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (TrimmedForm form) field =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.email then
                    ["email cannot be blank"]
                else
                    []

            Password ->
                if String.isEmpty form.password then
                    ["password can't be blank."]
                else
                    []


validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
        case List.concatMap (validateField trimmedForm) fieldsToValidate of
            [] -> Ok trimmedForm

            problems -> Err problems


-- HTTP

login : TrimmedForm -> Cmd Msg
login (TrimmedForm form) =
    let
        user =
            Encode.object
                [ ("email", Encode.string form.email)
                , ("password", Encode.string form.password)
                ]
        
        body =
            Encode.object [("user", user)]
                |> Http.jsonBody
    in
        Api.login body User.decoder CompletedLogin

-- Export

toSession : Model -> Session
toSession model =
    model.session
