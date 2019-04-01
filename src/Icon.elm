module Icon exposing
    ( calendarDay
    , check
    , clock
    , edit
    , ellipsis
    , google
    , grip
    , home
    , icon
    , left
    , list
    , magic
    , menu
    , right
    , search
    , signIn
    , signOut
    , stickyNote
    , toggle
    , toggleOff
    , toggleOn
    , twitter
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
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


magic : List (Element.Attribute msg) -> Element msg
magic =
    icon [ "fas", "fa-magic" ]


ellipsis : List (Element.Attribute msg) -> Element msg
ellipsis =
    icon [ "fas", "fa-ellipsis-h" ]


stickyNote : List (Element.Attribute msg) -> Element msg
stickyNote =
    icon [ "far", "fa-sticky-note" ]


grip : List (Element.Attribute msg) -> Element msg
grip =
    icon [ "fas", "fa-grip-vertical" ]


clock : List (Element.Attribute msg) -> Element msg
clock =
    icon [ "far", "fa-clock" ]


list : List (Element.Attribute msg) -> Element msg
list =
    icon [ "fas", "fa-list" ]


calendarDay : List (Element.Attribute msg) -> Element msg
calendarDay =
    icon [ "fas", "fa-calendar-day" ]


home : List (Element.Attribute msg) -> Element msg
home =
    icon [ "fas", "fa-home" ]


search : List (Element.Attribute msg) -> Element msg
search =
    icon [ "fas", "fa-search" ]


left : List (Element.Attribute msg) -> Element msg
left =
    icon [ "fas", "fa-chevron-left" ]


right : List (Element.Attribute msg) -> Element msg
right =
    icon [ "fas", "fa-chevron-right" ]


check : List (Element.Attribute msg) -> Element msg
check =
    icon [ "fas", "fa-check" ]


toggle : Bool -> List (Element.Attribute msg) -> Element msg
toggle bool =
    if bool then
        toggleOn

    else
        toggleOff


toggleOn : List (Element.Attribute msg) -> Element msg
toggleOn =
    icon [ "fas", "fa-toggle-on" ]


toggleOff : List (Element.Attribute msg) -> Element msg
toggleOff =
    icon [ "fas", "fa-toggle-off" ]


edit : List (Element.Attribute msg) -> Element msg
edit =
    icon [ "fas ", "fa-pen" ]


signIn : List (Element.Attribute msg) -> Element msg
signIn =
    icon [ "fas ", "fa-sign-in-alt" ]


signOut : List (Element.Attribute msg) -> Element msg
signOut =
    icon [ "fas ", "fa-sign-out-alt" ]


twitter : List (Element.Attribute msg) -> Element msg
twitter =
    icon [ "fab", "fa-twitter" ]


google : List (Element.Attribute msg) -> Element msg
google =
    icon [ "fab", "fa-google" ]


menu : List (Element.Attribute msg) -> Element msg
menu =
    icon [ "fas", "fa-bars" ]
