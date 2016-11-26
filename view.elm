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
            (case model.appState of
                LoadingState difficulty ->
                    []

                ActiveState gameState ->
                    (List.concat
                        [ drawFilters
                        , drawEdges gameState.nodes gameState.edges
                        , drawNodes (Dict.values gameState.nodes)
                        ]
                    )
            )
        ]


drawFilters : List (Html Msg)
drawFilters =
    [ (Svg.filter
        [ id "node-shadow" ]
        [ feGaussianBlur [ stdDeviation "2.5" ] [] ]
      )
    , (Svg.radialGradient
        [ id "node-fill" ]
        [ Svg.stop [ offset "75%", stopColor "rgba(255, 255, 255, 1.0)" ] []
        , Svg.stop [ offset "85%", stopColor "rgba(220, 220, 220, 0.7)" ] []
        , Svg.stop [ offset "90%", stopColor "rgba(200, 200, 200, 0.3)" ] []
        , Svg.stop [ offset "95%", stopColor "rgba(0, 0, 0, 0.1)" ] []
        , Svg.stop [ offset "100%", stopColor "rgba(0, 0, 0, 0)" ] []
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
            "url(#node-fill)"
    in
        [ {--ellipse
            [ cx (px realPosition.x)
            , cy (px realPosition.y)
            , rx (toString (realXRad * 1.15))
            , ry (toString (realYRad * 1.15))
            , transform (getTransform node)
            , Svg.Attributes.filter "url(#node-shadow)"
            , fill "black"
            ]
            []
            , --}
          ellipse
            [ onMouseDown
            , onMouseUp
            , cx (px realPosition.x)
            , cy (px realPosition.y)
            , rx (toString realXRad)
            , ry (toString realYRad)
            , transform (getTransform node)
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
