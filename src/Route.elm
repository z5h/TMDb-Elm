module Route exposing
    ( Route(..)
    , fromUrl
    , push
    , replace
    , routeParser
    , toString
    )

import Browser.Navigation as Navigation
import Dict
import Html
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as UrlParser exposing ((</>), (<?>))
import Url.Parser.Query as QueryParser


type Route
    = -- /
      Home
      -- /movie/<movie-code>
    | Movie Int
    | NotFound


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home
            UrlParser.top

        --
        , UrlParser.map Movie
            (UrlParser.s "movie" </> UrlParser.int)
        ]


toString : Route -> String
toString route =
    "#"
        ++ (case route of
                Home ->
                    Builder.absolute [] []

                Movie int ->
                    Builder.absolute [ "movie", String.fromInt int ] []

                NotFound ->
                    Builder.relative [ "notFound" ] []
           )


push : Navigation.Key -> Route -> Cmd msg
push key =
    toString >> Navigation.pushUrl key


replace : Navigation.Key -> Route -> Cmd msg
replace key =
    toString >> Navigation.replaceUrl key


fromUrl : Url -> Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    case String.split "?" (url.fragment |> Maybe.withDefault "") of
        path :: [] ->
            { url | path = path, fragment = Nothing }
                |> UrlParser.parse routeParser
                |> Maybe.withDefault NotFound

        path :: query :: [] ->
            { url | path = path, query = Just query, fragment = Nothing }
                |> UrlParser.parse routeParser
                |> Maybe.withDefault NotFound

        _ ->
            NotFound
