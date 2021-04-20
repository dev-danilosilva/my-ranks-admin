module Api.Endpoint exposing ( Endpoint(..)
                             , login
                             , buildUrl
                             )

import Url.Builder as UrlBuilder


type Endpoint = Endpoint String


baseUrl : String
baseUrl = ""


buildUrl : List String -> List UrlBuilder.QueryParameter -> Endpoint
buildUrl path queryParams =
    UrlBuilder.crossOrigin baseUrl ("api" :: path) queryParams
        |> Endpoint


login : Endpoint
login = buildUrl ["api", "login"] []
