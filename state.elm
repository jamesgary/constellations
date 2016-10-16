module State exposing (init, update, subscriptions)

import AnimationFrame
import Mouse
import Time


-- mine

import Types exposing (..)


baseWeight =
    50


init : ( Model, Cmd Msg )
init =
    ( { node = (Node (Pos 200 200) (Vel 0 0))
      , mouse = Mouse (Mouse.Position 200 200) False
      , now =
            0
            -- FIXME set to actual now
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        node =
            model.node

        mouse =
            model.mouse
    in
        ( case msg of
            MouseMove newMousePos ->
                { model | mouse = updateMousePos mouse newMousePos }

            MouseDown newMousePos ->
                { model
                    | mouse =
                        { pos = model.mouse.pos
                        , isPressed = True
                        }
                }

            MouseUp newMousePos ->
                { model
                    | mouse =
                        { pos = model.mouse.pos
                        , isPressed = False
                        }
                }

            AnimationMsg time ->
                animateEvent model time
        , Cmd.none
        )


updateMousePos : Mouse -> Mouse.Position -> Mouse
updateMousePos mouse newMousePos =
    { mouse | pos = newMousePos }


animateEvent : Model -> Time.Time -> Model
animateEvent model time =
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
