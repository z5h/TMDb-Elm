module Ux exposing
    ( button
    , buttonWithIcon
    , colorDarkBackground
    , colorDetailBackground
    , colorShadow
    , colorShadowNoAlpha
    , colorWhite
    , easeInRight
    , easeOutRight
    , ellipsis
    , focus
    , fontSizeDefault
    , fontSizeLarge
    , fontSizeSmall
    , hidden
    , hr
    , id
    , invert
    , menu
    , none
    , opacity
    , overflowHidden
    , positionFixed
    , separator
    , showIf
    , spaceBig
    , spaceMedium
    , spaceSmall
    , textInput
    , toGrey
    , underlineTextInput
    , withAppendableAttributes
    , withAttributes
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events
import Icon as Icon
import Json.Decode as Decode
import Json.Encode as Encode


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


colorShadowNoAlpha : Element.Color
colorShadowNoAlpha =
    Element.rgb 0.8 0.8 0.8


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
easeOutRight float =
    easeInRight (1.0 - float)


easeInRight : Float -> Element.Attribute msg
easeInRight float =
    let
        s =
            100.0 * (1 - float) |> round |> String.fromInt
    in
    Html.Attributes.style "transform" ("translate(" ++ s ++ "%)")
        |> Element.htmlAttribute


showIf : Bool -> Element.Attribute msg
showIf show =
    if show then
        none

    else
        hidden


invert : (Float -> Element.Attribute msg) -> (Float -> Element.Attribute msg)
invert f =
    \float -> f (1.0 - float)


opacity : Float -> Element.Attribute msg
opacity float =
    if float >= 1.0 then
        none

    else
        Html.Attributes.style "opacity" (String.fromFloat float)
            |> Element.htmlAttribute


id : String -> Element.Attribute msg
id =
    Html.Attributes.id >> Element.htmlAttribute


hidden : Element.Attribute msg
hidden =
    Html.Attributes.style "visibility" "hidden"
        |> Element.htmlAttribute


ellipsis : Element.Attribute msg
ellipsis =
    Html.Attributes.style "text-overflow" "ellipsis"
        |> Element.htmlAttribute


overflowHidden : Element.Attribute msg
overflowHidden =
    Html.Attributes.style "overflow" "hidden"
        |> Element.htmlAttribute


positionFixed : Element.Attribute msg
positionFixed =
    Html.Attributes.style "position" "fixed"
        |> Element.htmlAttribute


none : Element.Attribute msg
none =
    Html.Attributes.classList [] |> Element.htmlAttribute


hr : List (Element.Attribute msg) -> Element msg
hr attr =
    Element.el
        ([ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
         , Element.width Element.fill
         ]
            ++ attr
        )
        Element.none


buttonWithIcon :
    List (Element.Attribute msg)
    -> { onPress : Maybe msg, label : Maybe (Element msg), icon : Element msg }
    -> Element msg
buttonWithIcon attributes { onPress, label, icon } =
    button attributes
        { onPress = onPress
        , label =
            case label of
                Just justLabel ->
                    Element.row [ Element.width Element.fill, Element.spacing spaceMedium ]
                        [ Element.el [ Element.alignLeft ] justLabel
                        , Element.el [ Element.alignRight ] icon
                        ]

                Nothing ->
                    Element.row [ Element.width Element.fill, Element.spacing spaceMedium ]
                        [ Element.el [ Element.alignRight ] icon
                        ]
        }


textInput :
    List (Element.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
textInput =
    Input.text
        |> withAttributes
            [ Background.color colorWhite
            , Element.padding spaceMedium
            , Element.height <| Element.px (fontSizeDefault + spaceMedium * 2)
            , Element.htmlAttribute <| Html.Attributes.autocomplete False
            ]


underlineTextInput :
    List (Element.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
underlineTextInput =
    textInput
        |> withAttributes
            [ Border.rounded 0
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            , Element.focused []
            , Border.color colorElementBorder
            , Font.size fontSizeDefault
            ]


focus : String -> msg -> Element.Attribute msg
focus elementId msg =
    Element.el [ Element.transparent True ]
        (Html.node "focus-callback"
            [ Html.Attributes.property "callBackId" (Encode.string elementId)
            , Html.Events.on "attempted" (Decode.succeed msg)
            ]
            []
            |> Element.html
        )
        |> Element.onRight


button : List (Element.Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button =
    Input.button
        |> withAttributes
            [ Border.rounded 3
            , Border.color colorElementBorder
            , Border.width 2
            , Element.padding spaceMedium
            , Element.focused []
            ]


separator : Element.Color -> Element msg
separator color =
    Element.el
        [ Element.width Element.fill
        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        , Border.color color
        ]
        Element.none


withAttributes :
    List (Element.Attribute msg)
    -> (List (Element.Attribute msg) -> a)
    -> (List (Element.Attribute msg) -> a)
withAttributes attributes constructor =
    \newAttributes ->
        constructor (attributes ++ newAttributes)


withAppendableAttributes :
    (List (Element.Attribute msg) -> (properties -> element))
    -> (List (Element.Attribute msg) -> properties -> List (Element.Attribute msg) -> element)
withAppendableAttributes constructor =
    \attributes properties moreAttributes ->
        constructor (attributes ++ moreAttributes) properties
