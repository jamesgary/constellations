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
        mouse =
            model.mouse
    in
        case msg of
            MouseMove mousePos ->
                ( { model | mouse = updateMousePos mouse mousePos }
                , Cmd.none
                )

            MouseDown mousePos ->
                let
                    model =
                        { model
                            | mouse =
                                { pos = mousePos
                                , isPressed = True
                                }
                        }
                in
                    ( model, Cmd.none )

            MouseUp mousePos ->
                ( { model
                    | mouse =
                        { pos = mousePos
                        , isPressed = False
                        }
                  }
                , Cmd.none
                )

            AnimationMsg time ->
                ( (animate model time)
                , Cmd.none
                )


updateMousePos : Mouse -> Mouse.Position -> Mouse
updateMousePos mouse newMousePos =
    { mouse | pos = newMousePos }


animate : Model -> Time.Time -> Model
animate model time =
    let
        timeElapsed =
            time

        newPos =
            if model.mouse.isPressed then
                calculatePosition model.node model.mouse.pos timeElapsed
            else
                model.node.pos

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
