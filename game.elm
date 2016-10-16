module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App as App
import Html.Events exposing (on)
import AnimationFrame
import Json.Decode as Json
import Mouse
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


baseRadius =
    50


baseWeight =
    100


baseStretch =
    5


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { node : Node
    , mouse : Mouse
    , now : Time.Time
    }


type alias Node =
    { pos : Pos
    , vel : Vel
    }


type alias Mouse =
    { pos : Mouse.Position
    }


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Vel =
    { r : Float
    , a : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { node = (Node (Pos 200 200) (Vel 0 0))
      , mouse = Mouse (Mouse.Position 200 200)
      , now =
            0
            -- FIXME set to actual now
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MouseMove Mouse.Position
    | AnimationMsg Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        MouseMove newMousePos ->
            { model
                | mouse =
                    { pos = newMousePos
                    }
            }

        AnimationMsg time ->
            let
                timeElapsed =
                    time

                newPos =
                    calculatePosition model.node model.mouse.pos timeElapsed

                newVel =
                    calculateVel model.node newPos timeElapsed
            in
                { model
                    | node =
                        { pos = newPos
                        , vel = newVel
                        }
                    , now = time
                }
    , Cmd.none
    )


calculatePosition : Node -> Mouse.Position -> Time.Time -> Pos
calculatePosition node mousePos timeElapsed =
    let
        dragSpeed =
            1 - (baseWeight / (baseWeight + timeElapsed))

        mouseX =
            toFloat mousePos.x

        mouseY =
            toFloat mousePos.y

        distX =
            mouseX - node.pos.x

        distY =
            mouseY - node.pos.y

        newX =
            node.pos.x + (dragSpeed * distX)

        newY =
            node.pos.y + (dragSpeed * distY)
    in
        Pos newX newY


calculateVel : Node -> Pos -> Time.Time -> Vel
calculateVel node newPos timeElapsed =
    let
        xDiff =
            (newPos.x - node.pos.x) / timeElapsed

        yDiff =
            (newPos.y - node.pos.y) / timeElapsed

        ( r, a ) =
            toPolar ( xDiff, yDiff )
    in
        Vel r a



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs AnimationMsg
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        realPosition =
            model.node.pos

        stretch =
            baseStretch * model.node.vel.r

        realXRad =
            baseRadius + stretch

        realYRad =
            baseRadius * (baseRadius / realXRad)
    in
        svg
            [ width "800"
            , height "600"
            , viewBox "0 0 800 600"
            , onMouseMove
            ]
            [ ellipse
                [ cx (px realPosition.x)
                , cy (px realPosition.y)
                , rx (toString realXRad)
                , ry (toString realYRad)
                , transform (getTransform model.node)
                , fill "red"
                ]
                []
            ]


getTransform : Node -> String
getTransform node =
    let
        angleStr =
            toString (360 * (node.vel.a / (pi * 2)))

        xStr =
            toString node.pos.x

        yStr =
            toString node.pos.y
    in
        "rotate (" ++ angleStr ++ " " ++ xStr ++ " " ++ yStr ++ ")"


px : Float -> String
px number =
    toString number ++ "px"


onMouseMove : Attribute Msg
onMouseMove =
    on "mousemove" (Json.map MouseMove Mouse.position)
