module View exposing (view)

import Html exposing (Html, div, text)
import Html.Events exposing (on)
import Json.Decode as Json
import Mouse
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- mine

import Types exposing (..)


baseRadius =
    30


baseStretch =
    5


baseBlur =
    0.2


angleConvert =
    180 / pi


view : Model -> Html Msg
view model =
    let
        realPosition =
            model.node.pos

        stretch =
            baseStretch * model.node.vel.r

        blur =
            baseBlur * model.node.vel.r

        realXRad =
            baseRadius + stretch

        realYRad =
            baseRadius * (baseRadius / realXRad)

        color =
            if model.mouse.isPressed then
                "red"
            else
                "white"
    in
        svg
            [ width "100%"
            , height "100%"
            , Svg.Attributes.style "background: black"
            , onMouseMove
            , onMouseUp
            ]
            [ Svg.filter
                [ id "blur"
                ]
                [ feGaussianBlur
                    [ stdDeviation (toString blur)
                    ]
                    []
                ]
            , ellipse
                [ cx (px realPosition.x)
                , cy (px realPosition.y)
                , rx (toString realXRad)
                , ry (toString realYRad)
                , transform (getTransform model.node)
                , Svg.Attributes.filter "url(#blur)"
                , fill color
                , onMouseDown
                ]
                []
            ]


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
