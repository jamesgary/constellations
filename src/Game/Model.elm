module Game.Model exposing (Model, applyAspectRatio, getContainerDomCmd, goToLvl, handleWorkerMsg, init, mouseDown, mouseMove, mouseUp, save, tick, updateDom)

import Array exposing (Array)
import Browser.Dom
import Cfg
import Colors
import Dict exposing (Dict)
import Ease
import Edge exposing (Edge)
import Game.Mode as Mode exposing (Mode)
import Game.Msg as Msg exposing (Msg(..))
import Graph exposing (Graph)
import List.Extra
import LocalStorage exposing (LocalStorage)
import MouseState exposing (MouseState)
import Node exposing (Node)
import Ports
import Pos exposing (Pos)
import Random
import Set exposing (Set)
import Shape exposing (Shape)
import Task exposing (Task)
import Vel exposing (Vel)
import Worker.WorkerToAppMsg as WorkerToAppMsg exposing (WorkerToAppMsg)


type alias Model =
    { graph : Graph
    , mousePos : Pos
    , mouseState : MouseState
    , mode : Mode
    , canvasSize : ( Float, Float )
    , localStorage : LocalStorage
    , currentLvlIndex : Int
    }


init : LocalStorage -> Int -> ( Model, Cmd Msg )
init localStorage lvlIndex =
    let
        ( graph, mode, cmd ) =
            case Array.get lvlIndex localStorage.levels of
                Just lvl ->
                    ( lvl
                    , Mode.Playing
                    , [ Ports.getIntersections lvl
                      , getContainerDomCmd
                      ]
                        |> Cmd.batch
                    )

                Nothing ->
                    ( Graph.empty
                    , Mode.Loading 0
                    , [ Ports.loadLevel lvlIndex
                      , getContainerDomCmd
                      ]
                        |> Cmd.batch
                    )
    in
    ( { graph = graph
      , mousePos = Pos -9999 -9999
      , mouseState = MouseState.Default
      , mode = mode
      , canvasSize = ( 1, 1 )
      , localStorage = localStorage
      , currentLvlIndex = lvlIndex
      }
    , cmd
    )


getContainerDomCmd : Cmd Msg
getContainerDomCmd =
    Browser.Dom.getElement Cfg.constellationContainerId
        |> Task.attempt GotContainerDom


mouseMove : Pos -> Model -> ( Model, Cmd Msg )
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
            ( { model | mouseState = newMouseState }
            , Cmd.none
            )

        MouseState.Hovering _ ->
            let
                newMouseState =
                    case topTouchingNodeId of
                        Just ( nodeId, _ ) ->
                            MouseState.Hovering nodeId

                        Nothing ->
                            MouseState.Default
            in
            ( { model | mouseState = newMouseState }
            , Cmd.none
            )

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
            ( { model
                | graph =
                    model.graph
                        |> Graph.setNodeDest nodeId newDest
              }
            , Cmd.none
            )

        MouseState.Lassoing startPos _ nodeIds ->
            let
                lassoedNodes =
                    model.graph
                        |> Graph.getNodeIdsInBox startPos mousePos
            in
            ( { model
                | mouseState =
                    MouseState.Lassoing startPos mousePos lassoedNodes
              }
            , Cmd.none
            )

        MouseState.Lassoed nodeIds ->
            ( model
            , Cmd.none
            )

        MouseState.DraggingLassoed nodeOffsetDict ->
            let
                buffer =
                    Cfg.canvasScale * 0.1
            in
            ( { model
                | graph =
                    nodeOffsetDict
                        |> Dict.toList
                        |> List.foldl
                            (\( nodeId, offset ) graph ->
                                graph
                                    |> Graph.setNodeDest nodeId
                                        (Pos
                                            ((mousePos.x + offset.x) |> clamp buffer (Cfg.canvasScale - buffer))
                                            ((mousePos.y + offset.y) |> clamp buffer (Cfg.canvasScale - buffer))
                                        )
                            )
                            model.graph
              }
            , Cmd.none
            )


mouseDown : Pos -> Model -> ( Model, Cmd Msg )
mouseDown origMousePos model =
    let
        mousePos =
            mousePosToPos origMousePos

        ( newMouseState, cmd ) =
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
                                ( MouseState.DraggingLassoed nodeOffsetDict
                                , Cmd.none
                                )

                            else
                                -- clicked some rando non-lassoed node
                                let
                                    neighboringNodeIds =
                                        model.graph
                                            |> Graph.neighbors clickedNodeId
                                in
                                ( MouseState.Dragging clickedNodeId dragOffset neighboringNodeIds
                                , Cmd.none
                                )

                        _ ->
                            let
                                neighboringNodeIds =
                                    model.graph
                                        |> Graph.neighbors clickedNodeId
                            in
                            ( MouseState.Dragging clickedNodeId dragOffset neighboringNodeIds
                            , Cmd.none
                            )

                Nothing ->
                    ( MouseState.Lassoing mousePos mousePos Set.empty
                    , Cmd.none
                    )
    in
    ( { model | mouseState = newMouseState }
    , cmd
    )


mouseUp : Model -> ( Model, Cmd Msg )
mouseUp model =
    case model.mouseState of
        MouseState.Default ->
            -- should never get here
            ( model
            , Cmd.none
            )

        MouseState.Hovering nodeId ->
            -- should never get here
            ( model
            , Cmd.none
            )

        MouseState.Lassoed nodeIds ->
            -- should never get here
            ( model
            , Cmd.none
            )

        MouseState.Dragging nodeId offset nodeIds ->
            ( { model
                | mouseState =
                    MouseState.Hovering nodeId
              }
            , Ports.getIntersections model.graph
            )

        MouseState.Lassoing startPos currentPos nodeIds ->
            ( { model
                | mouseState =
                    if Set.isEmpty nodeIds then
                        MouseState.Default

                    else
                        MouseState.Lassoed nodeIds
              }
            , Cmd.none
            )

        MouseState.DraggingLassoed nodeOffsetDict ->
            ( { model
                | mouseState =
                    case Graph.getTouching model.mousePos model.graph of
                        Just ( nodeId, node ) ->
                            MouseState.Hovering nodeId

                        Nothing ->
                            MouseState.Default
              }
            , Ports.getIntersections model.graph
            )


mousePosToPos : Pos -> Pos
mousePosToPos { x, y } =
    Pos
        (Cfg.canvasScale * x)
        (Cfg.canvasScale * y)


tick : Float -> Model -> ( Model, Cmd Msg )
tick delta model =
    case model.mode of
        Mode.Loading age ->
            let
                newAge =
                    age + delta
            in
            ( { model
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
            , Cmd.none
            )

        Mode.Playing ->
            ( { model
                | graph =
                    model.graph
                        |> Graph.animateNodes delta
              }
            , Cmd.none
            )

        Mode.Won time shapes ->
            ( { model
                | graph =
                    model.graph
                        |> Graph.animateNodes delta
                , mode =
                    Mode.Won (time + delta) shapes
              }
            , Cmd.none
            )


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


handleWorkerMsg : WorkerToAppMsg -> Model -> ( Model, Cmd Msg )
handleWorkerMsg msg model =
    case msg of
        WorkerToAppMsg.GeneratedGraph { graph } ->
            ( { model | graph = graph }, Cmd.none )

        WorkerToAppMsg.GotIntersections { edges } ->
            let
                newModel =
                    if Set.isEmpty edges then
                        { model
                            | mode =
                                Mode.Won 0
                                    (getShapes model.graph)
                        }

                    else
                        { model | mode = Mode.Playing }
            in
            newModel
                |> save


updateDom : Browser.Dom.Element -> Model -> ( Model, Cmd Msg )
updateDom element model =
    ( { model
        | canvasSize =
            ( element.element.width
              -- use viewport's height
            , element.viewport.height
            )
      }
    , Cmd.none
    )


applyAspectRatio : Float -> Model -> Model
applyAspectRatio ratio model =
    { model
        | graph =
            model.graph
                |> Graph.applyAspectRatio ratio
        , mousePos =
            model.mousePos
                |> Pos.applyAspectRatio ratio
        , mouseState =
            model.mouseState
                |> MouseState.applyAspectRatio ratio
        , mode =
            model.mode
                |> Mode.applyAspectRatio ratio
    }


goToLvl : Int -> Model -> ( Model, Cmd Msg )
goToLvl lvlIndex model =
    let
        ls =
            toLocalStorage model
    in
    init ls lvlIndex


save : Model -> ( Model, Cmd Msg )
save model =
    let
        newLocalStorage =
            toLocalStorage model
    in
    ( { model
        | localStorage = newLocalStorage
      }
    , Ports.save newLocalStorage
    )


toLocalStorage : Model -> LocalStorage
toLocalStorage model =
    let
        newLevels =
            if Graph.isEmpty model.graph then
                -- no-op just in case it autosaves while loading a level
                model.localStorage.levels

            else if Array.length model.localStorage.levels > model.currentLvlIndex then
                model.localStorage.levels
                    |> Array.set model.currentLvlIndex model.graph

            else
                model.localStorage.levels
                    |> Array.push model.graph
    in
    { numLevelsCleared =
        case model.mode of
            Mode.Won _ _ ->
                max (model.currentLvlIndex + 1)
                    model.localStorage.numLevelsCleared

            _ ->
                model.localStorage.numLevelsCleared
    , levels = newLevels
    }



-- shape stuff


getShapes : Graph -> List Shape
getShapes graph =
    let
        nodes =
            Graph.getNodes graph

        edges =
            Graph.getEdges graph

        edgesList =
            edges
                |> Dict.values
    in
    edgesList
        |> List.map
            (\( id1, id2 ) ->
                let
                    node1 =
                        Graph.getNodeUnsafe id1 graph

                    node2 =
                        Graph.getNodeUnsafe id2 graph
                in
                [ getShapeNodeIdsForRay nodes edgesList ( ( id1, node1 ), ( id2, node2 ) )
                , getShapeNodeIdsForRay nodes edgesList ( ( id2, node2 ), ( id1, node1 ) )
                ]
            )
        |> List.concat
        -- reduce shapes to unique-ify
        |> List.Extra.uniqueBy
            (\nodeIds ->
                List.sort nodeIds
            )
        |> (\nodeIdsList ->
                let
                    perimeterShape =
                        List.Extra.maximumBy List.length nodeIdsList
                            |> Maybe.withDefault []
                in
                List.Extra.remove perimeterShape nodeIdsList
           )
        -- sort topright to bottomleft for shimmer
        |> List.sortBy
            (\nodeIds ->
                nodeIds
                    |> List.Extra.maximumBy
                        (\nodeId ->
                            Graph.getNodeUnsafe nodeId graph
                                |> (\{ pos } ->
                                        pos.y - pos.x
                                   )
                        )
                    |> Maybe.withDefault ""
            )
        |> List.indexedMap
            (\i nodeIds ->
                { pts =
                    nodeIds
                        |> List.map (\id -> Graph.getNodeUnsafe id graph)
                        |> List.map (\{ dest } -> dest)
                , color = getRandomColor (i * 1)
                , dimmerAnimationDurationMs =
                    Random.step
                        (Random.int 0 800)
                        (Random.initialSeed (i * 1))
                        |> Tuple.first
                        |> (\n -> n + 2000)
                , shimmerAnimationDelayMs = 70 * i
                }
            )


getShapeNodeIdsForRay : Dict Node.Id Node -> List Edge -> ( ( Node.Id, Node ), ( Node.Id, Node ) ) -> List Node.Id
getShapeNodeIdsForRay nodes edges (( ( node1Id, node1 ), ( node2Id, node2 ) ) as ray) =
    getShapeForRayHelper nodes edges node1 [ ( node2Id, node2 ), ( node1Id, node1 ) ] ray
        |> List.map Tuple.first


getShapeForRayHelper : Dict Node.Id Node -> List Edge -> Node -> List ( Node.Id, Node ) -> ( ( Node.Id, Node ), ( Node.Id, Node ) ) -> List ( Node.Id, Node )
getShapeForRayHelper nodes edges genesisNode shapeNodes ( ( node1Id, node1 ), ( node2Id, node2 ) ) =
    -- get neighbors of node2 (but not node1)
    getNeighborsOfNode nodes edges node2Id
        |> List.filter (\( id, n ) -> n /= node1)
        -- then find the ccw-most node of node2
        |> List.Extra.minimumBy (\( id, n ) -> angleFor3Pts node1.pos node2.pos n.pos)
        |> Maybe.map
            (\( nextNodeId, nextNode ) ->
                if nextNode == genesisNode then
                    shapeNodes

                else
                    getShapeForRayHelper nodes edges genesisNode (( nextNodeId, nextNode ) :: shapeNodes) ( ( node2Id, node2 ), ( nextNodeId, nextNode ) )
            )
        |> Maybe.withDefault []


getRandomColor : Int -> String
getRandomColor seed =
    Random.step Colors.starryNightColorGen (Random.initialSeed seed)
        |> Tuple.first


angleFor3Pts : Pos -> Pos -> Pos -> Float
angleFor3Pts pos1 pos2 pos3 =
    -- https://www.gamedev.net/forums/topic/487576-angle-between-two-lines-clockwise/?tab=comments#comment-4184664
    -- and https://stackoverflow.com/a/21484228
    let
        v1x =
            pos1.x - pos2.x

        v1y =
            pos1.y - pos2.y

        v2x =
            pos3.x - pos2.x

        v2y =
            pos3.y - pos2.y

        angleVal =
            atan2 v2y v2x - atan2 v1y v1x
    in
    if angleVal < 0 then
        angleVal + 2 * pi

    else
        angleVal


getNeighborsOfNode : Dict Node.Id Node -> List Edge -> Node.Id -> List ( Node.Id, Node )
getNeighborsOfNode nodes edges nodeId =
    edges
        |> List.filterMap
            (\( node1, node2 ) ->
                if node1 == nodeId then
                    Dict.get node2 nodes
                        |> Maybe.map (\node -> ( node2, node ))

                else if node2 == nodeId then
                    Dict.get node1 nodes
                        |> Maybe.map (\node -> ( node1, node ))

                else
                    Nothing
            )
