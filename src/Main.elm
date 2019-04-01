module Main exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import ComponentResult
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
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
    }


type alias Flags =
    { apiKey : String
    , width : Int
    , height : Int
    }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | MoviePageMsg MoviePage.Msg


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
            , width = flags.width
            , height = flags.height
            , moviePageModel = Nothing
            }

        ( result, cmd ) =
            newRoute model (Route.fromUrl url)
    in
    ( result, cmd )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            Element.text "content"
        ]
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
