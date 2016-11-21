module View exposing (view)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes
import Html.Events exposing (on)
import Json.Decode as Json
import Mouse
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- mine

import Types exposing (..)


baseStretch =
    5


baseBlur =
    0.8


angleConvert =
    180 / pi


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "background", "black" )
            ]
        ]
        [ svg
            [ width "100%"
            , height "100%"
            , Svg.Attributes.style "background: black"
            , onMouseMove
            , onMouseUp
            ]
            (List.concat
                [ (drawEdges model.nodes model.edges)
                , (drawNodes (Dict.values model.nodes))
                ]
            )
        ]


drawNodes : List Node -> List (Html Msg)
drawNodes nodesList =
    (List.concat (List.map drawNode nodesList))


drawNode : Node -> List (Html Msg)
drawNode node =
    let
        realPosition =
            node.pos

        stretch =
            baseStretch * node.vel.r

        blur =
            getBlur node

        rad =
            node.rad

        realXRad =
            rad + stretch

        realYRad =
            rad * (rad / realXRad)

        color =
            "white"
    in
        [ Svg.filter
            [ id ("blur-node-" ++ (toString node.id)) ]
            [ feGaussianBlur [ stdDeviation blur ] [] ]
        , ellipse
            [ onMouseDown
            , onMouseUp
            , cx (px realPosition.x)
            , cy (px realPosition.y)
            , rx (toString realXRad)
            , ry (toString realYRad)
            , transform (getTransform node)
            , Svg.Attributes.filter (("url(#blur-node-" ++ (toString node.id)) ++ ")")
            , fill color
            ]
            []
        ]


drawEdges : Dict Id Node -> List Edge -> List (Html Msg)
drawEdges nodes edges =
    (List.concat (List.map (drawEdge nodes) edges))


drawEdge : Dict Id Node -> Edge -> List (Html Msg)
drawEdge nodes edge =
    let
        node1 =
            getNode nodes (Tuple.first edge)

        node2 =
            getNode nodes (Tuple.second edge)
    in
        [ line
            [ x1 (toString node1.pos.x)
            , y1 (toString node1.pos.y)
            , x2 (toString node2.pos.x)
            , y2 (toString node2.pos.y)
            , strokeWidth "3"
            , stroke "rgba(255,255,255,.5)"
            ]
            []
        ]


getNode : Dict Id Node -> Id -> Node
getNode nodes id =
    case Dict.get id nodes of
        Just node ->
            node

        Nothing ->
            -- should never happen
            { id = -1
            , rad = 42
            , dest = Pos 42 42
            , pos = Pos 42 42
            , vel = Vel 0 0 0 0
            , isHovered = False
            , dragOffset = Nothing
            }


getTransform : Node -> String
getTransform node =
    let
        angleStr =
            toString (aToDegs node.vel.a)

        xStr =
            toString node.pos.x

        yStr =
            toString node.pos.y
    in
        "rotate (" ++ angleStr ++ " " ++ xStr ++ " " ++ yStr ++ ")"


getBlur : Node -> String
getBlur node =
    let
        xStr =
            toString (baseBlur * node.vel.x)

        yStr =
            toString (baseBlur * node.vel.y)
    in
        xStr ++ "," ++ yStr


aToDegs : Float -> Float
aToDegs a =
    angleConvert * a


px : Float -> String
px number =
    toString number ++ "px"


onMouseMove : Attribute Msg
onMouseMove =
    on "mousemove" (Json.map MouseMove Mouse.position)


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Json.map MouseDown Mouse.position)


onMouseUp : Attribute Msg
onMouseUp =
    on "mouseup" (Json.map MouseUp Mouse.position)
