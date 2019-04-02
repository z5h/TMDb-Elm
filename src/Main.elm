module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation exposing (Key)
import ComponentResult
import Data
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Http
import MoviePage
import Route exposing (Route)
import Url exposing (Url)
import Ux


type alias Model =
    { apiKey : String
    , width : Int
    , height : Int
    , moviePageModel : Maybe MoviePage.Model
    , key : Key
    , route : Route
    , url : Url
    , errors : List String
    , loading : Bool
    , nextPage : Int
    , movies : List Data.Movie
    }


type alias Flags =
    { apiKey : String
    , width : Int
    , height : Int
    }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | GotPopularResponse (Result Http.Error Data.PopularResponse)
    | Error String
    | MoviePageMsg MoviePage.Msg
    | Resize Int Int
    | LoadMore


newRoute : Model -> Route -> ( Model, Cmd Msg )
newRoute model route =
    ( model, Cmd.none )


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { apiKey = flags.apiKey
            , key = key
            , url = url
            , route = Route.Home
            , errors = []
            , loading = False
            , nextPage = 1
            , width = flags.width
            , height = flags.height
            , moviePageModel = Nothing
            , movies = []
            }

        ( result, cmd ) =
            newRoute model (Route.fromUrl url)
    in
    ( result, Cmd.batch [ cmd, getMovies model ] )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


subscriptions model =
    Sub.batch
        [ Browser.Events.onResize Resize
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize w h ->
            ( { model | width = w, height = h }, Cmd.none )

        Error string ->
            ( { model | errors = string :: model.errors }, Cmd.none )

        LoadMore ->
            ( model, getMovies model )

        GotPopularResponse result ->
            case result of
                Ok popularResponse ->
                    ( { model
                        | nextPage = popularResponse.page + 1
                        , movies = model.movies ++ popularResponse.results
                      }
                    , Cmd.none
                    )

                Err httpError ->
                    let
                        errorMsg =
                            case httpError of
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
                    in
                    update (Error errorMsg) model

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External str ->
                    ( model, Cmd.none )

        OnUrlChange url ->
            let
                route =
                    Route.fromUrl url
            in
            newRoute { model | route = route, url = url } route

        MoviePageMsg moviePageMsg ->
            case model.moviePageModel of
                Just moviePageModel ->
                    MoviePage.update moviePageMsg moviePageModel
                        |> ComponentResult.applyExternalMsg
                            (\externalMsg result ->
                                case externalMsg of
                                    MoviePage.ExternalMsg ->
                                        result
                            )
                        |> ComponentResult.mapModel
                            (\updatedMoviePageModel ->
                                { model | moviePageModel = Just updatedMoviePageModel }
                            )
                        |> ComponentResult.mapMsg MoviePageMsg
                        |> ComponentResult.resolve

                Nothing ->
                    ComponentResult.withModel model
                        |> ComponentResult.resolve


getMovies : { a | apiKey : String, nextPage : Int } -> Cmd Msg
getMovies { apiKey, nextPage } =
    Http.get
        { url = "https://api.themoviedb.org/3/movie/popular?api_key=" ++ apiKey ++ "&page=" ++ String.fromInt nextPage
        , expect = Http.expectJson GotPopularResponse Data.popularResponseDecoder
        }


view : Model -> Browser.Document Msg
view model =
    { title = "TMDb"
    , body =
        [ Element.layout
            [ Element.inFront (menu "Movies")
            , Element.height Element.fill
            , Font.size Ux.fontSizeDefault
            ]
          <|
            Element.wrappedRow [] <|
                (model.movies |> List.map (viewMovie model))
                    ++ [ Ux.button [] { onPress = Just LoadMore, label = Element.text "Load More" } ]
        ]
    }


viewMovie : Model -> Data.Movie -> Element Msg
viewMovie model movieData =
    Element.link []
        { url = ""
        , label =
            Element.el
                [ Element.width <| Element.px <| (model.width // 2 - 1)
                ]
            <|
                Element.image [ Element.width <| Element.px <| model.width // 2 ]
                    { src =
                        "http://image.tmdb.org/t/p/w500" ++ movieData.posterPath
                    , description = ""
                    }
        }


menu : String -> Element msg
menu title =
    Element.el
        [ Element.padding Ux.spaceMedium
        , Element.width Element.fill
        , Background.color Ux.colorDarkBackground
        , Font.color Ux.colorWhite
        ]
    <|
        Element.text title
