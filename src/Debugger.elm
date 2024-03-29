module Debugger exposing (debugger)

-- mine

import Html exposing (Html, div, table, td, text, tr)
import Html.Attributes exposing (style)
import Types exposing (..)


debugger : Model -> Html Msg
debugger { nodes, mouse, now } =
    div
        [ Html.Attributes.style "width" "500px"
        , Html.Attributes.style "height" "100%"
        , Html.Attributes.style "background" "gray"
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "right" "0"
        , Html.Attributes.style "bottom" "0"
        , Html.Attributes.style "font-family" "Consolas, monospace"
        , Html.Attributes.style "user-select" "none"
        , Html.Attributes.style "-webkit-user-select" "none"
        ]
        [ table []
            [ {--debugRow "node.rad" node.rad
                , debugRow "node.dest.x" node.dest.x
                , debugRow "node.dest.y" node.dest.y
                , debugRow "node.pos.x" node.pos.x
                , debugRow "node.pos.y" node.pos.y
                , debugRow "node.vel.r" node.vel.r
                , debugRow "node.vel.a" node.vel.a
                , debugRow "node.isHovered" node.isHovered
                , debugRow "---" "---"
                ,--}
              debugRow "mouse.pos.x" mouse.pos.x
            , debugRow "mouse.pos.y" mouse.pos.y
            ]
        ]


debugRow : String -> a -> Html Msg
debugRow label val =
    tr []
        [ td [ style "font-weight" "bold" ] [ Html.text label ]
        , td [] [ Html.text (toString val) ]
        ]
