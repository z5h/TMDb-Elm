module Data exposing
    ( Movie
    , PopularResponse
    , getMoviesUrl
    , imageData
    , movieDecoder
    , popularResponseDecoder
    )

import Json.Decode as Decode exposing (Decoder)


imageData : Movie -> { src : String, description : String }
imageData movie =
    { src = "http://image.tmdb.org/t/p/w342" ++ movie.posterPath
    , description = "Poster for " ++ movie.originalTitle
    }


getMoviesUrl : { a | apiKey : String, nextApiResultsPage : Int } -> String
getMoviesUrl { apiKey, nextApiResultsPage } =
    "https://api.themoviedb.org/3/movie/popular?api_key="
        ++ apiKey
        ++ "&page="
        ++ String.fromInt nextApiResultsPage


type alias Movie =
    { id : Int
    , originalTitle : String
    , posterPath : String
    , overview : String
    , voteAverage : Float
    , releaseYear : Int
    }


movieDecoder : Decode.Decoder Movie
movieDecoder =
    Decode.map6 Movie
        (Decode.field "id" Decode.int)
        (Decode.field "original_title" Decode.string)
        (Decode.field "poster_path" Decode.string)
        (Decode.field "overview" Decode.string)
        (Decode.field "vote_average" Decode.float)
        (Decode.field "release_date" Decode.string
            |> Decode.andThen
                (\string ->
                    string
                        |> String.split "-"
                        |> List.head
                        |> Maybe.andThen String.toInt
                        |> Maybe.map Decode.succeed
                        |> Maybe.withDefault (Decode.fail string)
                )
        )


type alias PopularResponse =
    { page : Int
    , results : List Movie
    }


popularResponseDecoder : Decoder PopularResponse
popularResponseDecoder =
    Decode.map2 PopularResponse
        (Decode.field "page" Decode.int)
        (Decode.field "results" (Decode.list movieDecoder))
