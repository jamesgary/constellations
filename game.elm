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
    , velocity : Float
    }


type alias Mouse =
    { pos : Mouse.Position
    }


type alias Pos =
    { x : Float
    , y : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { node = (Node (Pos 200 200) 0)
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

                --time - model.now
                newPos =
                    calculatePosition model.node model.mouse.pos timeElapsed

                newVelocity =
                    calculateVelocity model.node newPos timeElapsed
            in
                { model
                    | node =
                        { pos = newPos
                        , velocity = newVelocity
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

        --0.1
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


calculateVelocity : Node -> Pos -> Time.Time -> Float
calculateVelocity node newPos timeElapsed =
    0



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs AnimationMsg
        ]



--Sub.none
-- VIEW


view : Model -> Html Msg
view model =
    let
        realPosition =
            model.node.pos

        --getPosition model
    in
        svg
            [ width "800"
            , height "600"
            , viewBox "0 0 800 600"
            , onMouseMove
            ]
            [ circle
                [ cx (px realPosition.x)
                , cy (px realPosition.y)
                , r (toString (baseRadius + model.node.velocity))
                , fill "red"
                ]
                []
            ]


px : Float -> String
px number =
    toString number ++ "px"


onMouseMove : Attribute Msg
onMouseMove =
    on "mousemove" (Json.map MouseMove Mouse.position)
