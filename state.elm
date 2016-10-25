module State exposing (init, update, subscriptions)

import AnimationFrame
import Time
import Mouse


-- mine

import Types exposing (..)


baseWeight =
    50


baseRadius =
    50


init : ( Model, Cmd Msg )
init =
    ( { node =
            { rad = baseRadius
            , dest = (Pos 200 200)
            , pos = (Pos 200 200)
            , vel = (Vel 0 0)
            , isHovered = False
            }
      , mouse =
            { pos = (Pos 0 0)
            , dragOffset = Nothing
            }
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

        node =
            model.node
    in
        case msg of
            MouseMove mousePos ->
                let
                    newPos =
                        mousePosToPos mousePos

                    newMouse =
                        { mouse
                            | pos = newPos
                        }

                    newModel =
                        { model | mouse = newMouse }
                in
                    ( newModel, Cmd.none )

            MouseDown mousePos ->
                let
                    newPos =
                        mousePosToPos mousePos

                    newMouse =
                        { mouse
                            | pos = newPos
                            , dragOffset =
                                if isTouching node newPos then
                                    Just (Pos (node.pos.x - newPos.x) (node.pos.y - newPos.y))
                                else
                                    Nothing
                        }

                    newModel =
                        { model | mouse = newMouse }
                in
                    ( newModel, Cmd.none )

            MouseUp mousePos ->
                let
                    newPos =
                        mousePosToPos mousePos

                    newMouse =
                        { mouse
                            | pos = newPos
                            , dragOffset = Nothing
                        }

                    newModel =
                        { model | mouse = newMouse }
                in
                    ( newModel, Cmd.none )

            AnimationMsg time ->
                ( (animate model time), Cmd.none )


animate : Model -> Time.Time -> Model
animate model timeElapsed =
    let
        node =
            model.node

        mouse =
            model.mouse

        newDest =
            case mouse.dragOffset of
                Just dragOffset ->
                    let
                        destX =
                            mouse.pos.x + dragOffset.x

                        destY =
                            mouse.pos.y + dragOffset.y
                    in
                        Pos destX destY

                Nothing ->
                    node.dest

        newNode =
            { node | dest = newDest }

        newNewNode =
            moveNodeToDest newNode timeElapsed

        -- TODO isHovered
    in
        { model
            | node = newNewNode
        }


moveNodeToDest : Node -> Time.Time -> Node
moveNodeToDest node timeElapsed =
    let
        newPos =
            calculatePosition node timeElapsed

        newVel =
            calculateVel node newPos timeElapsed
    in
        { node
            | pos = newPos
            , vel = newVel
        }


isTouching : Node -> Pos -> Bool
isTouching node mousePos =
    let
        aSquared =
            (node.pos.x - mousePos.x) ^ 2

        bSquared =
            (node.pos.y - mousePos.y) ^ 2

        c =
            sqrt (aSquared + bSquared)
    in
        c < node.rad


calculatePosition : Node -> Time.Time -> Pos
calculatePosition node timeElapsed =
    let
        dragSpeed =
            1 - (baseWeight / (baseWeight + timeElapsed))

        mouseX =
            node.dest.x

        mouseY =
            node.dest.y

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


mousePosToPos : Mouse.Position -> Pos
mousePosToPos mousePos =
    Pos (toFloat mousePos.x) (toFloat mousePos.y)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs AnimationMsg
        ]
