module Util.Request exposing ( RequestConfig
                             , RequestConfigBuilder
                             , request
                             , config
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
request reqConfig =
    Http.request
        { method  = reqConfig.method
        , body    = reqConfig.body
        , headers = reqConfig.headers
        , url     = reqConfig.url
        , timeout = reqConfig.timeout
        , expect  = reqConfig.expect
        , tracker = reqConfig.tracker
        }

config :  Http.Expect msg -> RequestConfig msg
config expectation =
    { method  = "GET"
    , body    = Http.emptyBody
    , headers = []
    , url     = ""
    , timeout = Nothing
    , tracker = Nothing
    , expect = expectation
    }


method : String -> RequestConfigBuilder msg
method met reqConfig= { reqConfig| method = met }


body : Http.Body -> RequestConfigBuilder msg
body bd reqConfig= { reqConfig| body = bd }


headers : List Http.Header -> RequestConfigBuilder msg
headers hs reqConfig=
    { reqConfig| headers = hs }


url : String -> RequestConfigBuilder msg
url addr reqConfig= { reqConfig| url = addr }


timeout : Float -> RequestConfigBuilder msg
timeout to reqConfig= { reqConfig| timeout = Just to }


expect : Http.Expect msg -> RequestConfigBuilder msg
expect e reqConfig= { reqConfig| expect = e }


tracker : Maybe String -> RequestConfigBuilder msg
tracker t reqConfig= { reqConfig| tracker = t  } 
