module Game.Model exposing (Model, init, mouseDown, mouseMove, mouseUp, tick)

import Cfg
import Dict exposing (Dict)
import Ease
import Game.Mode as Mode exposing (Mode)
import Graph exposing (Graph)
import List.Extra
import MouseState exposing (MouseState)
import Node exposing (Node)
import Pos exposing (Pos)
import Set exposing (Set)
import Shape exposing (Shape)
import Vel exposing (Vel)


type alias Model =
    { graph : Graph
    , nodeDests : Dict Node.Id Pos
    , nodeVels : Dict Node.Id Vel
    , mousePos : Pos
    , mouseState : MouseState
    , mode : Mode
    }


init : Graph -> Model
init graph =
    { graph = graph
    , nodeDests = Dict.empty
    , nodeVels = Dict.empty
    , mousePos = Pos -9999 -9999
    , mouseState = MouseState.Default
    , mode = Mode.Loading 0
    }


mouseMove : Pos -> Model -> Model
mouseMove origMousePos origModel =
    let
        mousePos =
            mousePosToPos origMousePos

        model =
            { origModel | mousePos = mousePos }

        topTouchingNodeId =
            Graph.getTouching mousePos model.graph
    in
    case model.mouseState of
        MouseState.Default ->
            let
                newMouseState =
                    case topTouchingNodeId of
                        Just ( nodeId, _ ) ->
                            MouseState.Hovering nodeId

                        Nothing ->
                            MouseState.Default
            in
            { model | mouseState = newMouseState }

        MouseState.Hovering _ ->
            let
                newMouseState =
                    case topTouchingNodeId of
                        Just ( nodeId, _ ) ->
                            MouseState.Hovering nodeId

                        Nothing ->
                            MouseState.Default
            in
            { model | mouseState = newMouseState }

        MouseState.Dragging nodeId offset neighboringNodeIds ->
            let
                draggedNode =
                    Graph.getNodeUnsafe nodeId model.graph

                destX =
                    mousePos.x + offset.x

                destY =
                    mousePos.y + offset.y

                newDest =
                    Pos destX destY
            in
            { model
                | graph =
                    model.graph
                        |> Graph.setNodeDest nodeId newDest
            }

        MouseState.Lassoing startPos _ nodeIds ->
            let
                lassoedNodes =
                    model.graph
                        |> Graph.getNodeIdsInBox startPos mousePos
            in
            { model
                | mouseState =
                    MouseState.Lassoing startPos mousePos lassoedNodes
            }

        MouseState.Lassoed nodeIds ->
            model

        MouseState.DraggingLassoed nodeOffsetDict ->
            let
                buffer =
                    10
            in
            { model
                | graph =
                    nodeOffsetDict
                        |> Dict.toList
                        |> List.foldl
                            (\( nodeId, offset ) graph ->
                                graph
                                    |> Graph.setNodeDest nodeId
                                        (Pos
                                            ((mousePos.x + offset.x) |> clamp buffer (1600 - buffer))
                                            ((mousePos.y + offset.y) |> clamp buffer (900 - buffer))
                                        )
                            )
                            model.graph
            }


mouseDown : Pos -> Model -> Model
mouseDown origMousePos model =
    let
        mousePos =
            mousePosToPos origMousePos

        newMouseState =
            case Graph.getTouching mousePos model.graph of
                Just ( clickedNodeId, clickedNode ) ->
                    let
                        dragOffset =
                            Pos
                                (clickedNode.pos.x - mousePos.x)
                                (clickedNode.pos.y - mousePos.y)
                    in
                    case model.mouseState of
                        MouseState.Lassoed nodeIds ->
                            if Set.member clickedNodeId nodeIds then
                                -- clicked a lassoed node
                                let
                                    nodeOffsetDict =
                                        nodeIds
                                            --|> List.map (nodeIdToNodeOffset mousePos model.nodes)
                                            |> Set.toList
                                            |> List.filterMap
                                                (\nodeId ->
                                                    Graph.getNode nodeId model.graph
                                                        |> Maybe.map
                                                            (\node ->
                                                                ( nodeId
                                                                , Pos
                                                                    (node.pos.x - mousePos.x)
                                                                    (node.pos.y - mousePos.y)
                                                                )
                                                            )
                                                )
                                            |> Dict.fromList
                                in
                                MouseState.DraggingLassoed nodeOffsetDict

                            else
                                -- clicked some rando non-lassoed node
                                let
                                    getNeighborNodeIfMatchingEdge nodeId edge =
                                        let
                                            ( node1, node2 ) =
                                                edge.pair
                                        in
                                        if node1 == nodeId then
                                            Just node2

                                        else if node2 == nodeId then
                                            Just node1

                                        else
                                            Nothing

                                    neighboringNodeIds =
                                        []
                                            --model.edges
                                            --|> List.filterMap (getNeighborNodeIfMatchingEdge clickedNodeId)
                                            |> always []
                                            |> Set.fromList
                                in
                                MouseState.Dragging clickedNodeId dragOffset neighboringNodeIds

                        _ ->
                            let
                                getNeighborNodeIfMatchingEdge nodeId edge =
                                    let
                                        ( node1, node2 ) =
                                            edge.pair
                                    in
                                    if node1 == nodeId then
                                        Just node2

                                    else if node2 == nodeId then
                                        Just node1

                                    else
                                        Nothing

                                neighboringNodeIds =
                                    --List.filterMap (getNeighborNodeIfMatchingEdge clickedNodeId) model.edges
                                    []
                                        |> Set.fromList
                            in
                            MouseState.Dragging clickedNodeId dragOffset neighboringNodeIds

                Nothing ->
                    MouseState.Lassoing mousePos mousePos Set.empty
    in
    { model | mouseState = newMouseState }


mouseUp : Model -> Model
mouseUp model =
    -- TODO check for win state
    case model.mouseState of
        MouseState.Default ->
            -- should never get here
            model

        MouseState.Hovering nodeId ->
            -- should never get here
            model

        MouseState.Lassoed nodeIds ->
            -- should never get here
            model

        MouseState.Dragging nodeId offset nodeIds ->
            { model
                | mouseState =
                    MouseState.Hovering nodeId
            }

        MouseState.Lassoing startPos currentPos nodeIds ->
            { model
                | mouseState =
                    MouseState.Lassoed nodeIds
            }

        MouseState.DraggingLassoed nodeOffsetDict ->
            { model
                | mouseState =
                    case Graph.getTouching model.mousePos model.graph of
                        Just ( nodeId, node ) ->
                            MouseState.Hovering nodeId

                        Nothing ->
                            MouseState.Default
            }


mousePosToPos : Pos -> Pos
mousePosToPos { x, y } =
    Pos
        (1600 * x)
        (900 * y)


tick : Float -> Model -> Model
tick delta model =
    case model.mode of
        Mode.Loading age ->
            let
                newAge =
                    age + delta
            in
            { model
                | graph =
                    model.graph
                        |> Graph.moveNodesForLoadAnim newAge
                        |> Graph.animateNodes delta
                , mode =
                    if age < Cfg.loadAnimDur then
                        Mode.Loading newAge

                    else
                        Mode.Playing
            }

        Mode.Playing ->
            { model
                | graph =
                    model.graph
                        |> Graph.animateNodes delta
            }

        Mode.Won time shapes ->
            { model
                | graph =
                    model.graph
                        |> Graph.animateNodes delta
                , mode =
                    Mode.Won (time + delta) shapes
            }


animateNode : Float -> Node.Id -> Node -> Node
animateNode timeElapsed _ node =
    let
        newPos =
            let
                dragSpeed =
                    1 - (Cfg.baseWeight / (Cfg.baseWeight + timeElapsed))

                distX =
                    node.dest.x - node.pos.x

                distY =
                    node.dest.y - node.pos.y

                newX =
                    node.pos.x + (dragSpeed * distX)

                newY =
                    node.pos.y + (dragSpeed * distY)
            in
            Pos newX newY

        newVel =
            let
                xDiff =
                    (newPos.x - node.pos.x) / timeElapsed

                yDiff =
                    (newPos.y - node.pos.y) / timeElapsed

                ( r, a ) =
                    toPolar ( xDiff, yDiff )
            in
            Vel xDiff yDiff r a
    in
    { node
        | pos = newPos
        , vel = newVel
    }


moveNodeForLoadAnim : Float -> Int -> Node.Id -> Node -> Node
moveNodeForLoadAnim time numNodes id node =
    let
        age =
            if time < Cfg.wait then
                0

            else
                min Cfg.loadAnimDur (time - Cfg.wait)

        ease =
            Ease.outElastic (age / Cfg.loadAnimDur)

        easeRot =
            Ease.outCubic (age / Cfg.loadAnimDur)

        easeInv =
            1 - ease

        index =
            id
                |> String.toFloat
                |> Maybe.withDefault -1

        rotation =
            (index / toFloat numNodes) + (easeRot * 0.1)

        destX =
            Cfg.graphCenterX + cos (2 * pi * rotation) * Cfg.graphRadius

        destY =
            Cfg.graphCenterY + sin (2 * pi * rotation) * Cfg.graphRadius
    in
    { node
        | dest =
            Pos (ease * destX + easeInv * Cfg.graphCenterX)
                (ease * destY + easeInv * Cfg.graphCenterY)
    }
