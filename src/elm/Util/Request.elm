module Util.Request exposing ( RequestConfig
                             , RequestConfigBuilder
                             , request
                             , configRequest
                             , method
                             , body
                             , headers
                             , url
                             , timeout
                             , tracker
                             , expect
                             )


import Http


type alias RequestConfig msg =
    { method  : String
    , body    : Http.Body
    , headers : List Http.Header
    , url     : String
    , timeout : Maybe Float
    , tracker : Maybe String
    , expect  : Http.Expect msg
    }

type alias RequestConfigBuilder msg
    = RequestConfig msg -> RequestConfig msg


request : RequestConfig msg -> Cmd msg
request config =
    Http.request
        { method  = config.method
        , body    = config.body
        , headers = config.headers
        , url     = config.url
        , timeout = config.timeout
        , expect  = config.expect
        , tracker = config.tracker
        }

configRequest :  Http.Expect msg -> RequestConfig msg
configRequest expectation =
    { method  = "GET"
    , body    = Http.emptyBody
    , headers = []
    , url     = ""
    , timeout = Nothing
    , tracker = Nothing
    , expect = expectation
    }


method : String -> RequestConfigBuilder msg
method met config = { config | method = met }


body : Http.Body -> RequestConfigBuilder msg
body bd config = { config | body = bd }


headers : List Http.Header -> RequestConfigBuilder msg
headers hs config =
    { config | headers = hs }


url : String -> RequestConfigBuilder msg
url addr config = { config | url = addr }


timeout : Float -> RequestConfigBuilder msg
timeout to config = { config | timeout = Just to }


expect : Http.Expect msg -> RequestConfigBuilder msg
expect e config = { config | expect = e }


tracker : Maybe String -> RequestConfigBuilder msg
tracker t config = { config | tracker = t  } 
