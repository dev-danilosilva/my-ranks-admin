module Session exposing (..)

import Browser.Navigation as Nav
import User exposing (User)
import Api exposing (Credential)

type Session
    = LoggedIn Nav.Key User
    | Guest Nav.Key

currentUser : Session -> Maybe User
currentUser session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing

cred : Session -> Maybe Credential
cred session =
    case session of
        LoggedIn _ val ->
            Just (User.cred val)

        Guest _ ->
            Nothing

navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key

changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.userChanges (\maybeUser -> toMsg (fromCurrentUser key maybeUser)) User.decoder

fromCurrentUser : Nav.Key -> Maybe User -> Session
fromCurrentUser key maybeUser =
    case maybeUser of
        Just userVal -> LoggedIn key userVal
        Nothing      -> Guest    key