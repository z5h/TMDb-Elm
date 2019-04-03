module Ux exposing
    ( colorDarkBackground
    , colorDetailBackground
    , colorShadow
    , colorWhite
    , easeInRight
    , easeOutRight
    , fontSizeDefault
    , fontSizeLarge
    , fontSizeSmall
    , hidden
    , id
    , invert
    , menu
    , none
    , overflowHidden
    , spaceBig
    , spaceMedium
    , spaceSmall
    , toGrey
    , withAttributes
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes


colorDarkBackground : Element.Color
colorDarkBackground =
    Element.rgb255 0x21 0x21 0x21


colorWhite : Element.Color
colorWhite =
    Element.rgb 1.0 1.0 1.0


colorDetailBackground : Element.Color
colorDetailBackground =
    Element.rgb255 0x00 0x96 0x88


colorElementBorder : Element.Color
colorElementBorder =
    Element.rgb255 0x6F 0xCA 0xC2


toGrey : Float -> Element.Color
toGrey f =
    Element.rgb f f f


colorShadow : Element.Color
colorShadow =
    Element.rgba 0 0 0 0.5


spaceBig : Int
spaceBig =
    20


spaceMedium : Int
spaceMedium =
    10


spaceSmall : Int
spaceSmall =
    5


fontSizeLarge : Int
fontSizeLarge =
    30


fontSizeDefault : Int
fontSizeDefault =
    20


fontSizeSmall : Int
fontSizeSmall =
    15


menu : List (Element.Attribute msg) -> Element msg -> Element msg
menu attributes content =
    Element.el
        ([ Element.paddingXY 24 18
         , Element.width Element.fill
         , Background.color colorDarkBackground
         , Font.color colorWhite
         , Font.family [ Font.typeface "Helvetica" ]
         , Font.size 24
         , Font.bold
         ]
            ++ attributes
        )
        content


easeOutRight : Float -> Element.Attribute msg
easeOutRight =
    invert easeInRight


easeInRight : Float -> Element.Attribute msg
easeInRight float =
    let
        s =
            100.0 * (1 - float) |> round |> String.fromInt
    in
    Html.Attributes.style "transform" ("translate(" ++ s ++ "%)")
        |> Element.htmlAttribute


invert : (Float -> Element.Attribute msg) -> (Float -> Element.Attribute msg)
invert f =
    \float -> f (1.0 - float)


id : String -> Element.Attribute msg
id =
    Html.Attributes.id >> Element.htmlAttribute


hidden : Element.Attribute msg
hidden =
    Html.Attributes.style "visibility" "hidden"
        |> Element.htmlAttribute


overflowHidden : Element.Attribute msg
overflowHidden =
    Html.Attributes.style "overflow" "hidden"
        |> Element.htmlAttribute


none : Element.Attribute msg
none =
    Html.Attributes.classList [] |> Element.htmlAttribute


withAttributes :
    List (Element.Attribute msg)
    -> (List (Element.Attribute msg) -> a)
    -> (List (Element.Attribute msg) -> a)
withAttributes attributes constructor =
    \newAttributes ->
        constructor (attributes ++ newAttributes)
