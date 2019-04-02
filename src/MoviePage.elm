module MoviePage exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , empty
    , initWithMovie
    , update
    , view
    )

import ComponentResult exposing (ComponentResult)
import Data
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Icon
import Ux


type Model
    = Movie Data.Movie
    | Empty


type Msg
    = ExitMoviePage_


type ExternalMsg
    = ExitMoviePage


initWithMovie : Data.Movie -> ComponentResult Model msg externalMsg err
initWithMovie movie =
    ComponentResult.withModel (Movie movie)


empty : Model
empty =
    Empty


update : Msg -> Model -> ComponentResult Model Msg ExternalMsg err
update msg model =
    case msg of
        ExitMoviePage_ ->
            ComponentResult.withModel model
                |> ComponentResult.withExternalMsg ExitMoviePage


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color Ux.colorWhite
        ]
    <|
        [ Ux.menu
            [ Events.onClick ExitMoviePage_ ]
            (Element.row [ Element.spacing Ux.spaceMedium ]
                [ Icon.left []
                , Element.text <| "Movie Detail"
                ]
            )
        ]
            ++ (case model of
                    Movie movieData ->
                        [ body movieData ]

                    Empty ->
                        []
               )


body : Data.Movie -> Element Msg
body movieData =
    Element.column
        [ Element.spacing 24
        ]
    <|
        [ Element.el
            [ Background.color Ux.colorDetailBackground
            , Element.width Element.fill
            , Font.size 32
            , Element.paddingXY 24 21
            , Border.shadow { offset = ( 0, 2 ), size = 1, blur = 8, color = Ux.colorShadow }
            , Font.color Ux.colorWhite
            , Font.light
            ]
          <|
            Element.text movieData.originalTitle
        , Element.row
            [ Element.spacing 24
            , Element.paddingXY 24 0
            , Font.family [ Font.typeface "Helvetica" ]
            , Font.size 24
            , Font.light
            ]
            [ Element.image [ Element.width <| Element.fillPortion 1 ] <| Data.imageData movieData
            , Element.column
                [ Element.width <| Element.fillPortion 1
                , Element.height Element.fill
                , Element.spacing Ux.spaceSmall
                ]
                [ Element.text <| String.fromInt movieData.releaseYear
                , Element.text <| String.fromFloat movieData.voteAverage
                ]
            ]
        , Element.paragraph
            [ Element.paddingXY 24 0
            , Font.family [ Font.typeface "Helvetica" ]
            , Element.width Element.fill
            ]
            [ Element.text movieData.overview ]
        ]
