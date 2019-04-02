module Data exposing
    ( Movie
    , PopularResponse
    , movieDecoder
    , popularResponseDecoder
    )

import Json.Decode as Decode exposing (Decoder)


type alias Movie =
    { id : Int
    , originalTitle : String
    , posterPath : String
    , overview : String
    , voteAverage : Float
    }


movieDecoder : Decode.Decoder Movie
movieDecoder =
    Decode.map5 Movie
        (Decode.field "id" Decode.int)
        (Decode.field "original_title" Decode.string)
        (Decode.field "poster_path" Decode.string)
        (Decode.field "overview" Decode.string)
        (Decode.field "vote_average" Decode.float)


type alias PopularResponse =
    { page : Int
    , results : List Movie
    }


popularResponseDecoder : Decoder PopularResponse
popularResponseDecoder =
    Decode.map2 PopularResponse
        (Decode.field "page" Decode.int)
        (Decode.field "results" (Decode.list movieDecoder))
