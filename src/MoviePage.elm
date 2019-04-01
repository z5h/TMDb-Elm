module MoviePage exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , init
    , update
    , view
    )

import ComponentResult exposing (ComponentResult)
import Element exposing (Element)


type alias Model =
    ()


type Msg
    = Msg


type ExternalMsg
    = ExternalMsg


init : String -> ComponentResult Model Msg ExternalMsg err
init string =
    ComponentResult.withModel ()


update : Msg -> Model -> ComponentResult Model Msg ExternalMsg err
update msg model =
    ComponentResult.withModel model


view : { a | a : () } -> Model -> Element Msg
view _ _ =
    Element.none
