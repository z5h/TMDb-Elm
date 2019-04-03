module Icon exposing
    ( icon
    , left
    )

import Element exposing (Element)
import Html.Attributes


classAttribute : List String -> Element.Attribute msg
classAttribute classNames =
    classNames
        |> List.map (\className -> ( className, True ))
        |> Html.Attributes.classList
        |> Element.htmlAttribute


icon : List String -> List (Element.Attribute msg) -> Element msg
icon classNames attributes =
    Element.row
        ([ Element.width <| Element.shrink
         , Element.height <| Element.shrink
         , classAttribute classNames
         ]
            ++ attributes
        )
        []


left : List (Element.Attribute msg) -> Element msg
left =
    icon [ "fas", "fa-chevron-left" ]
