module Debugger exposing (debugger)

import Html exposing (Html, div, text, table, tr, td)
import Html.Attributes exposing (style)


-- mine

import Types exposing (..)


debugger : Model -> Html Msg
debugger model =
    let
        nodes =
            model.nodes

        mouse =
            model.mouse

        now =
            model.now
    in
        div
            [ Html.Attributes.style
                [ ( "width", "500px" )
                , ( "height", "100%" )
                , ( "background", "gray" )
                , ( "position", "absolute" )
                , ( "right", "0" )
                , ( "bottom", "0" )
                , ( "font-family", "Consolas, monospace" )
                , ( "user-select", "none" )
                , ( "-webkit-user-select", "none" )
                ]
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
        [ td [ style [ ( "font-weight", "bold" ) ] ] [ Html.text label ]
        , td [] [ Html.text (toString val) ]
        ]
