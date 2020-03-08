module Icons
    exposing
        ( edit2
        , eye
        , eyeOff
        , maximize
        , maximize2
        , minimize
        , minimize2
        , shuffle
        , star
        , sun
        , x
        )

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "24"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "24"
        ]


edit2 : Html msg
edit2 =
    svgFeatherIcon "edit-2"
        [ Svg.path [ d "M17 3a2.828 2.828 0 1 1 4 4L7.5 20.5 2 22l1.5-5.5L17 3z" ] []
        ]


eye : Html msg
eye =
    svgFeatherIcon "eye"
        [ Svg.path [ d "M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z" ] []
        , Svg.circle [ cx "12", cy "12", r "3" ] []
        ]


eyeOff : Html msg
eyeOff =
    svgFeatherIcon "eye-off"
        [ Svg.path [ d "M17.94 17.94A10.07 10.07 0 0 1 12 20c-7 0-11-8-11-8a18.45 18.45 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 8 11 8a18.5 18.5 0 0 1-2.16 3.19m-6.72-1.07a3 3 0 1 1-4.24-4.24" ] []
        , Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        ]


maximize : Html msg
maximize =
    svgFeatherIcon "maximize"
        [ Svg.path [ d "M8 3H5a2 2 0 0 0-2 2v3m18 0V5a2 2 0 0 0-2-2h-3m0 18h3a2 2 0 0 0 2-2v-3M3 16v3a2 2 0 0 0 2 2h3" ] []
        ]


maximize2 : Html msg
maximize2 =
    svgFeatherIcon "maximize-2"
        [ Svg.polyline [ points "15 3 21 3 21 9" ] []
        , Svg.polyline [ points "9 21 3 21 3 15" ] []
        , Svg.line [ x1 "21", y1 "3", x2 "14", y2 "10" ] []
        , Svg.line [ x1 "3", y1 "21", x2 "10", y2 "14" ] []
        ]


minimize : Html msg
minimize =
    svgFeatherIcon "minimize"
        [ Svg.path [ d "M8 3v3a2 2 0 0 1-2 2H3m18 0h-3a2 2 0 0 1-2-2V3m0 18v-3a2 2 0 0 1 2-2h3M3 16h3a2 2 0 0 1 2 2v3" ] []
        ]


minimize2 : Html msg
minimize2 =
    svgFeatherIcon "minimize-2"
        [ Svg.polyline [ points "4 14 10 14 10 20" ] []
        , Svg.polyline [ points "20 10 14 10 14 4" ] []
        , Svg.line [ x1 "14", y1 "10", x2 "21", y2 "3" ] []
        , Svg.line [ x1 "3", y1 "21", x2 "10", y2 "14" ] []
        ]


shuffle : Html msg
shuffle =
    svgFeatherIcon "shuffle"
        [ Svg.polyline [ points "16 3 21 3 21 8" ] []
        , Svg.line [ x1 "4", y1 "20", x2 "21", y2 "3" ] []
        , Svg.polyline [ points "21 16 21 21 16 21" ] []
        , Svg.line [ x1 "15", y1 "15", x2 "21", y2 "21" ] []
        , Svg.line [ x1 "4", y1 "4", x2 "9", y2 "9" ] []
        ]


star : Html msg
star =
    svgFeatherIcon "star"
        [ Svg.polygon [ points "12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2" ] []
        ]


sun : Html msg
sun =
    svgFeatherIcon "sun"
        [ Svg.circle [ cx "12", cy "12", r "5" ] []
        , Svg.line [ x1 "12", y1 "1", x2 "12", y2 "3" ] []
        , Svg.line [ x1 "12", y1 "21", x2 "12", y2 "23" ] []
        , Svg.line [ x1 "4.22", y1 "4.22", x2 "5.64", y2 "5.64" ] []
        , Svg.line [ x1 "18.36", y1 "18.36", x2 "19.78", y2 "19.78" ] []
        , Svg.line [ x1 "1", y1 "12", x2 "3", y2 "12" ] []
        , Svg.line [ x1 "21", y1 "12", x2 "23", y2 "12" ] []
        , Svg.line [ x1 "4.22", y1 "19.78", x2 "5.64", y2 "18.36" ] []
        , Svg.line [ x1 "18.36", y1 "5.64", x2 "19.78", y2 "4.22" ] []
        ]


x : Html msg
x =
    svgFeatherIcon "x"
        [ Svg.line [ x1 "18", y1 "6", x2 "6", y2 "18" ] []
        , Svg.line [ x1 "6", y1 "6", x2 "18", y2 "18" ] []
        ]