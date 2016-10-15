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


baseDragSpeed =
    0.35


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
      , mouse =
            Mouse (Mouse.Position 200 200)
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
                newPos =
                    calculatePosition model.node model.mouse.pos

                newVelocity =
                    calculateVelocity model.node model.mouse.pos
            in
                { model
                    | node =
                        { pos = newPos
                        , velocity = newVelocity
                        }
                }
    , Cmd.none
    )


calculatePosition : Node -> Mouse.Position -> Pos
calculatePosition node mousePos =
    let
        newX =
            node.pos.x - (baseDragSpeed * (node.pos.x - toFloat mousePos.x))

        newY =
            node.pos.y - (baseDragSpeed * (node.pos.y - toFloat mousePos.y))
    in
        Pos newX newY


calculateVelocity : Node -> Mouse.Position -> Float
calculateVelocity node mousePos =
    0



--toFloat (newPos.x - node.pos.x)
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
