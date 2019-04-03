module Main exposing (main)

import Base.Animations as Animations exposing (Animations)
import Base.Util as Util
import Browser
import Browser.Events
import Browser.Navigation exposing (Key)
import ComponentResult
import Data
import Element exposing (Element)
import Element.Events as Events
import Element.Font as Font
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Animations.subscriptions model.animations |> Sub.map AnimationMsg
        ]


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
                        , Animations.animated
                            model.animations
                            MovieDetailOpen
                            Ux.easeInRight
                            |> Maybe.withDefault Ux.none
                        , Animations.animated
                            model.animations
                            MovieDetailClose
                            Ux.easeOutRight
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
                [ Element.width <| Element.px <| (model.width // 2)
                ]
            <|
                Element.image [ Element.width <| Element.px <| model.width // 2 ] <|
                    Data.imageData movieData
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                    ( { model
                        | errors = Util.httpErrorString httpError :: model.errors
                        , loading = False
                      }
                    , Cmd.none
                    )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External _ ->
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


newRoute : Model -> Route -> ( Model, Cmd Msg )
newRoute model route =
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
            ( model, goHome model )


goHome : { a | key : Key } -> Cmd msg
goHome model =
    Route.push model.key Route.Home


getMovies : { a | apiKey : String, nextApiResultsPage : Int } -> Cmd Msg
getMovies params =
    Http.get
        { url = Data.getMoviesUrl params
        , expect =
            Http.expectJson
                GotPopularResponse
                Data.popularResponseDecoder
        }
