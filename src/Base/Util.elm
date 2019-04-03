module Base.Util exposing (httpErrorString)

import Http


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadUrl string ->
            "bad url " ++ string

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus int ->
            "bad status " ++ String.fromInt int

        Http.BadBody string ->
            "bad body " ++ string
