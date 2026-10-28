module Icons exposing (arrowDown, check, clock, close, play, plus, skip, trash, trophy, users)

import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Svg exposing (Svg, circle, path, svg)
import Svg.Attributes exposing (class, cx, cy, d, fill, height, r, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox, width)


icon : List (Svg msg) -> Html msg
icon =
    svg
        [ viewBox "0 0 24 24"
        , width "24"
        , height "24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "icon"
        , attribute "aria-hidden" "true"
        ]


play : Html msg
play =
    icon [ path [ d "M7 4.5v15a1 1 0 0 0 1.5.86l12-7.5a1 1 0 0 0 0-1.72l-12-7.5A1 1 0 0 0 7 4.5z" ] [] ]


plus : Html msg
plus =
    icon [ path [ d "M12 5v14M5 12h14" ] [] ]


trash : Html msg
trash =
    icon [ path [ d "M3 6h18M8 6V4h8v2M19 6l-1 14H6L5 6M10 11v6M14 11v6" ] [] ]


check : Html msg
check =
    icon [ path [ d "M20 6 9 17l-5-5" ] [] ]


close : Html msg
close =
    icon [ path [ d "M18 6 6 18M6 6l12 12" ] [] ]


skip : Html msg
skip =
    icon [ path [ d "M5 4.5v15l10-7.5zM19 5v14" ] [] ]


clock : Html msg
clock =
    icon
        [ circle [ cx "12", cy "12", r "9" ] []
        , path [ d "M12 7v5l3 2" ] []
        ]


users : Html msg
users =
    icon
        [ circle [ cx "9", cy "8", r "4" ] []
        , path [ d "M2 21v-1a6 6 0 0 1 6-6h2a6 6 0 0 1 6 6v1M16 3.13a4 4 0 0 1 0 7.75M22 21v-1a4 4 0 0 0-3-3.85" ] []
        ]


trophy : Html msg
trophy =
    icon [ path [ d "M8 21h8M12 17v4M7 4h10v5a5 5 0 0 1-10 0zM17 5h3v2a3 3 0 0 1-3 3M7 5H4v2a3 3 0 0 0 3 3" ] [] ]


arrowDown : Html msg
arrowDown =
    icon [ path [ d "M12 5v14M19 12l-7 7-7-7" ] [] ]
