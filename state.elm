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
    ( { nodes =
            [ { id = 0
              , rad = baseRadius
              , dest = (Pos 300 300)
              , pos = (Pos 300 300)
              , vel = (Vel 0 0)
              , isHovered = False
              , dragOffset = Nothing
              }
            , { id = 1
              , rad = baseRadius
              , dest = (Pos 400 200)
              , pos = (Pos 400 200)
              , vel = (Vel 0 0)
              , isHovered = False
              , dragOffset = Nothing
              }
            ]
      , draggedNode = Nothing
      , mouse =
            { pos = (Pos 0 0)
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

        nodes =
            model.nodes
    in
        case msg of
            MouseMove mousePos ->
                let
                    newPos =
                        mousePosToPos mousePos

                    newMouse =
                        { mouse | pos = newPos }

                    newModel =
                        { model | mouse = newMouse }
                in
                    ( newModel, Cmd.none )

            MouseDown mousePos ->
                let
                    newPos =
                        mousePosToPos mousePos

                    newMouse =
                        { mouse | pos = newPos }

                    processMouseDown pos node =
                        let
                            ( dragOffset, newDest ) =
                                if isTouching pos node then
                                    ( Just (Pos (node.pos.x - newPos.x) (node.pos.y - newPos.y))
                                    , pos
                                    )
                                else
                                    ( Nothing, node.dest )
                        in
                            { node
                                | dragOffset = dragOffset
                                , dest = newDest
                            }

                    newNodes =
                        List.map (processMouseDown newPos) nodes

                    newModel =
                        { model
                            | mouse = newMouse
                            , nodes = newNodes
                        }
                in
                    ( newModel, Cmd.none )

            MouseUp mousePos ->
                let
                    newPos =
                        mousePosToPos mousePos

                    newMouse =
                        { mouse
                            | pos = newPos
                        }

                    processMouseUp pos node =
                        let
                            newDest =
                                case node.dragOffset of
                                    Just dragOffset ->
                                        Pos (pos.x + dragOffset.x) (pos.y + dragOffset.y)

                                    Nothing ->
                                        node.dest
                        in
                            { node
                                | dragOffset = Nothing
                                , dest = newDest
                            }

                    newNodes =
                        List.map (processMouseUp newPos) nodes

                    newModel =
                        { model
                            | nodes = newNodes
                            , mouse = newMouse
                        }
                in
                    ( newModel, Cmd.none )

            AnimationMsg time ->
                ( (animate time model), Cmd.none )


animate : Time.Time -> Model -> Model
animate timeElapsed model =
    let
        nodes =
            model.nodes

        mouse =
            model.mouse

        newNodes =
            List.map (animateNode timeElapsed mouse.pos) nodes

        -- TODO isHovered
    in
        { model
            | nodes = newNodes
        }


animateNode : Time.Time -> Pos -> Node -> Node
animateNode timeElapsed mousePos node =
    let
        newDest =
            case node.dragOffset of
                Just dragOffset ->
                    let
                        destX =
                            mousePos.x + dragOffset.x

                        destY =
                            mousePos.y + dragOffset.y
                    in
                        Pos destX destY

                Nothing ->
                    node.dest

        newNode =
            { node | dest = newDest }

        newNewNode =
            moveNodeToDest newNode timeElapsed
    in
        newNewNode


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


isTouching : Pos -> Node -> Bool
isTouching mousePos node =
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
