module Main exposing (main)

import Base.Animations as Animations exposing (Animations)
import Browser
import Browser.Events
import Browser.Navigation exposing (Key)
import ComponentResult
import Data
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Http
import MoviePage
import Route exposing (Route)
import Url exposing (Url)
import Ux


type Animatable
    = MovieDetailOpen
    | MovieDetailClose


type alias Model =
    { apiKey : String
    , width : Int
    , height : Int
    , showMoviePage : Bool
    , moviePageModel : MoviePage.Model
    , key : Key
    , route : Route
    , url : Url
    , errors : List String
    , loading : Bool
    , nextApiResultsPage : Int
    , movies : List Data.Movie
    , animations : Animations Animatable
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
    | AnimationMsg (Animations.Msg Animatable)
    | Resize Int Int
    | LoadMore


newRoute : Model -> Route -> ( Model, Cmd Msg )
newRoute model route =
    let
        _ =
            Debug.log "newRoute" route
    in
    case route of
        Route.Home ->
            Animations.animate MovieDetailClose 300 model.animations
                |> ComponentResult.mapMsg AnimationMsg
                |> ComponentResult.mapModel
                    (\newAnimations ->
                        { model
                            | animations = newAnimations
                            , showMoviePage = False
                        }
                    )
                |> ComponentResult.resolve

        Route.Movie int ->
            let
                maybeMovie =
                    model.movies
                        |> List.filter
                            (\movieData -> movieData.id == int)
                        |> List.head

                _ =
                    Debug.log "maybeMovie" maybeMovie
            in
            case maybeMovie of
                Nothing ->
                    ( model, Cmd.none )

                Just movie ->
                    let
                        newAnimationResult =
                            Animations.animate MovieDetailOpen 300 model.animations
                                |> ComponentResult.mapMsg AnimationMsg

                        newPageResult =
                            MoviePage.initWithMovie movie
                                |> ComponentResult.mapMsg MoviePageMsg
                    in
                    ComponentResult.map2Model
                        (\animationModel pageModel ->
                            { model
                                | animations = animationModel
                                , moviePageModel = pageModel
                                , showMoviePage = True
                            }
                        )
                        newAnimationResult
                        newPageResult
                        |> ComponentResult.resolve

        Route.NotFound ->
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
            , nextApiResultsPage = 1
            , width = flags.width
            , height = flags.height
            , movies = []
            , animations = Animations.init
            , showMoviePage = False
            , moviePageModel = MoviePage.empty
            }
    in
    ( model, Cmd.batch [ Route.push model.key (Route.fromUrl url), getMovies model ] )


goHome : { a | key : Key } -> Cmd msg
goHome model =
    Route.push model.key Route.Home


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Animations.subscriptions model.animations |> Sub.map AnimationMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            case msg of
                AnimationMsg _ ->
                    msg

                _ ->
                    Debug.log "msg" msg
    in
    case msg of
        AnimationMsg animationMsg ->
            Animations.update animationMsg model.animations
                |> ComponentResult.mapModel (\newAnimations -> { model | animations = newAnimations })
                |> ComponentResult.mapMsg AnimationMsg
                |> ComponentResult.resolve

        Resize w h ->
            ( { model | width = w, height = h }, Cmd.none )

        Error string ->
            ( { model | errors = string :: model.errors }, Cmd.none )

        LoadMore ->
            let
                cmd =
                    if model.loading then
                        Cmd.none

                    else
                        getMovies model
            in
            ( model, cmd )

        GotPopularResponse result ->
            case result of
                Ok popularResponse ->
                    ( { model
                        | nextApiResultsPage = popularResponse.page + 1
                        , movies = model.movies ++ popularResponse.results
                        , loading = False
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
                    update (Error errorMsg) { model | loading = False }

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
            MoviePage.update moviePageMsg model.moviePageModel
                |> ComponentResult.applyExternalMsg
                    (\externalMsg result ->
                        case externalMsg of
                            MoviePage.ExitMoviePage ->
                                result |> ComponentResult.withCmd (goHome model)
                    )
                |> ComponentResult.mapModel
                    (\updatedMoviePageModel ->
                        { model | moviePageModel = updatedMoviePageModel }
                    )
                |> ComponentResult.mapMsg MoviePageMsg
                |> ComponentResult.resolve


getMovies : { a | apiKey : String, nextApiResultsPage : Int } -> Cmd Msg
getMovies { apiKey, nextApiResultsPage } =
    Http.get
        { url =
            "https://api.themoviedb.org/3/movie/popular?api_key="
                ++ apiKey
                ++ "&page="
                ++ String.fromInt nextApiResultsPage
        , expect =
            Http.expectJson
                GotPopularResponse
                Data.popularResponseDecoder
        }


view : Model -> Browser.Document Msg
view model =
    { title = "TMDb"
    , body =
        [ Element.layout
            [ Element.inFront (Ux.menu [] <| Element.text "Pop Movies")
            , Element.height Element.fill
            , Font.size Ux.fontSizeDefault
            , if model.showMoviePage || Animations.isAnimating model.animations MovieDetailClose then
                Element.inFront <|
                    Element.el
                        [ Element.width <| Element.px model.width
                        , Element.height <| Element.px model.height
                        , Animations.animated model.animations MovieDetailOpen Ux.easeInRight
                            |> Maybe.withDefault Ux.none
                        , Animations.animated model.animations MovieDetailClose Ux.easeOutRight
                            |> Maybe.withDefault Ux.none
                        ]
                    <|
                        (MoviePage.view model.moviePageModel
                            |> Element.map MoviePageMsg
                        )

              else
                Ux.none
            ]
          <|
            Element.column []
                [ Element.wrappedRow [] <|
                    (model.movies |> List.map (viewMovie model))
                , Ux.menu
                    [ Events.onClick LoadMore ]
                    (Element.text "Load More")
                ]
        ]
    }


viewMovie : Model -> Data.Movie -> Element Msg
viewMovie model movieData =
    Element.link []
        { url = Route.toString (Route.Movie movieData.id)
        , label =
            Element.el
                [ Element.width <| Element.px <| (model.width // 2 - 1)
                ]
            <|
                Element.image [ Element.width <| Element.px <| model.width // 2 ]
                    { src =
                        "http://image.tmdb.org/t/p/w342" ++ movieData.posterPath
                    , description = ""
                    }
        }
