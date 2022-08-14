module App exposing (getNode, init, subscriptions, update)

import AppState exposing (ActiveStateData, AppState)
import Array
import Browser
import Browser.Events
import Cfg
import Color
import Colors
import Config exposing (Config)
import Dict exposing (Dict)
import Ease
import Edge exposing (Edge)
import Flags exposing (Flags)
import GameMode exposing (GameMode)
import IntersectionResultData exposing (IntersectionResultData)
import List.Extra
import Model exposing (Model)
import MousePos exposing (MousePos)
import MouseState exposing (MouseState)
import Msg exposing (Msg(..))
import Node exposing (Node)
import Ports
    exposing
        ( checkForIntersections
        , intersectionResults
        , loadLevel
        , loadedLevelFresh
        , loadedLevelInProgress
        , saveConfig
        )
import Pos exposing (Pos)
import Random
import Random.Extra
import Random.List
import Shape exposing (Shape)
import Vel exposing (Vel)


baseWeight =
    50



-- INIT


init : Flags -> ( Model, Cmd Msg )
init { radius, showStella, levelsCleared, timestamp } =
    let
        hash =
            "#todo"
    in
    case hash of
        "#won" ->
            ( { appState = AppState.dummyWon
              , levelsCleared = levelsCleared
              , seed = Random.initialSeed timestamp

              --, lastLevelProgress = lastLevelProgress
              , config =
                    { radius = radius
                    , showStella = showStella
                    }
              }
            , Cmd.none
            )

        _ ->
            ( { appState = AppState.Start
              , levelsCleared = levelsCleared
              , seed = Random.initialSeed timestamp

              --, lastLevelProgress = lastLevelProgress
              , config =
                    { radius = radius
                    , showStella = showStella
                    }
              }
            , Cmd.none
            )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove mousePos ->
            updateMouseMove model mousePos

        MouseDown mousePos ->
            updateMouseDown model mousePos

        MouseUp mousePos ->
            updateMouseUp model mousePos

        AnimationMsg time ->
            updateAnimation model time

        -- response from js-land
        LoadedLevelFresh ( edges_, numNodes_, difficulty ) ->
            let
                ( edges, numNodes, newSeed ) =
                    makeGraphEdges difficulty model.seed
            in
            ( { model
                | appState =
                    AppState.Active
                        { nodes =
                            List.range 0 (numNodes - 1)
                                |> List.indexedMap (makeNode model.config numNodes)
                                |> Dict.fromList
                        , edges = edges
                        , difficulty = difficulty
                        , mouseState = MouseState.Default
                        , mode = GameMode.Loading 0
                        }
                , seed = newSeed
              }
            , Cmd.none
            )

        LoadedLevelInProgress ( { nodes, edges }, difficulty ) ->
            ( { model
                | appState =
                    AppState.Active
                        { nodes =
                            nodes
                                |> Array.toList
                                |> List.map (\n -> ( n.id, { n | pos = n.dest } ))
                                |> Dict.fromList
                        , edges = edges
                        , difficulty = difficulty
                        , mouseState = MouseState.Default
                        , mode = GameMode.Playing
                        }
              }
            , Cmd.none
            )

        GetIntersectionResults intersectionResultData ->
            updateGetIntersectionResults model intersectionResultData

        ClickedGoToLevel difficulty ->
            ( model, loadLevel difficulty )


makeGraphEdges : Int -> Random.Seed -> ( List Edge, Int, Random.Seed )
makeGraphEdges difficulty seed =
    {-
        1 2x2
        2 2x3
        3 3x3
        4 3x4
        5 4x4
        6 4x5
        7 5x5
        8 5x6
        9 6x6
       10 6x7
       11 7x7
       12 7x8
       13 8x8
       14 8x9
       15 9x9
    -}
    let
        ( h, w ) =
            ( ((difficulty - 1) // 2) + 2, (difficulty // 2) + 2 )

        numNodes =
            h * w

        maxNumNodes =
            round <| 0.9 * toFloat numNodes

        ( shuffledList, newSeed ) =
            List.range 0 (numNodes - 1)
                |> Random.List.shuffle
                |> (\g -> Random.step g seed)

        randomMap =
            shuffledList
                |> List.indexedMap (\i n -> ( i, n ))
                |> Dict.fromList
    in
    List.range 0 (numNodes - 1)
        |> List.foldl
            (\i ( edges, seed_ ) ->
                let
                    ( x, y ) =
                        ( modBy w i, i // w )

                    ( randBool, newSeed_ ) =
                        Random.step (Random.uniform True [ False ]) seed_
                in
                edges
                    |> (\edges_ ->
                            -- up
                            if y > 0 then
                                ( i, i - w ) :: edges_

                            else
                                edges_
                       )
                    |> (\edges_ ->
                            -- right
                            if x < (w - 1) then
                                ( i, i + 1 ) :: edges_

                            else
                                edges_
                       )
                    |> (\edges_ ->
                            -- down
                            if y < (h - 1) then
                                ( i, i + w ) :: edges_

                            else
                                edges_
                       )
                    |> (\edges_ ->
                            -- left
                            if x > 0 then
                                ( i, i - 1 ) :: edges_

                            else
                                edges_
                       )
                    |> (\edges_ ->
                            -- diagonal
                            if x < (w - 1) && y < (h - 1) then
                                if randBool then
                                    -- down-right from node
                                    ( i, i + w + 1 ) :: edges_

                                else
                                    -- down-left from right node
                                    ( i + 1, i + w ) :: edges_

                            else
                                edges_
                       )
                    |> (\edges_ -> ( edges_, newSeed_ ))
            )
            ( [], newSeed )
        |> (\( edges, seed_ ) ->
                ( edges
                    |> List.map
                        (\( a, b ) ->
                            ( Dict.get a randomMap |> Maybe.withDefault -1
                            , Dict.get b randomMap |> Maybe.withDefault -1
                            )
                        )
                    |> List.filter
                        (\( a, b ) ->
                            a < maxNumNodes && b < maxNumNodes
                        )
                    |> List.indexedMap
                        (\i e ->
                            { id = i
                            , pair = e
                            }
                        )
                , maxNumNodes
                , seed_
                )
           )


updateMouseMove : Model -> MousePos -> ( Model, Cmd Msg )
updateMouseMove model newMousePos =
    case model.appState of
        AppState.Active ({ mouseState, nodes } as gameState) ->
            let
                newPos =
                    mousePosToPos newMousePos

                newGameState =
                    case mouseState of
                        MouseState.Default ->
                            let
                                topTouchingNodeId =
                                    getTopTouchingNodeId model.config newPos nodes

                                newMouseState =
                                    case topTouchingNodeId of
                                        Just nodeId ->
                                            MouseState.Hovering nodeId

                                        Nothing ->
                                            MouseState.Default
                            in
                            { gameState | mouseState = newMouseState }

                        MouseState.Hovering _ ->
                            let
                                topTouchingNodeId =
                                    getTopTouchingNodeId model.config newPos nodes

                                newMouseState =
                                    case topTouchingNodeId of
                                        Just nodeId ->
                                            MouseState.Hovering nodeId

                                        Nothing ->
                                            MouseState.Default
                            in
                            { gameState | mouseState = newMouseState }

                        MouseState.Dragging nodeId offset neighboringNodeIds ->
                            let
                                draggedNode =
                                    getNode nodes nodeId

                                destX =
                                    newPos.x + offset.x

                                destY =
                                    newPos.y + offset.y

                                newDest =
                                    Pos destX destY

                                newDraggedNode =
                                    { draggedNode | dest = newDest }

                                newNodes =
                                    Dict.insert nodeId newDraggedNode nodes
                            in
                            { gameState | nodes = newNodes }

                        MouseState.Lassoing startPos _ nodeIds ->
                            let
                                lassoedNodes =
                                    List.filterMap (nodeInBoxFilterMap startPos newPos) (Dict.values nodes)

                                newMouseState =
                                    MouseState.Lassoing startPos newPos lassoedNodes
                            in
                            { gameState | mouseState = newMouseState }

                        MouseState.Lassoed nodeIds ->
                            gameState

                        MouseState.DraggingLassoed nodeOffsetList ->
                            let
                                newNodes =
                                    List.foldr (moveNodeOffset newPos) nodes nodeOffsetList
                            in
                            { gameState | nodes = newNodes }

                newModel =
                    { model | appState = AppState.Active newGameState }
            in
            ( newModel, Cmd.none )

        AppState.Start ->
            ( model, Cmd.none )


moveNodeOffset : Pos -> ( Node.Id, Pos ) -> Dict Node.Id Node -> Dict Node.Id Node
moveNodeOffset mousePos ( nodeId, offset ) nodes =
    let
        buffer =
            10

        node =
            getNode nodes nodeId

        destX =
            mousePos.x + offset.x

        destY =
            mousePos.y + offset.y

        -- make sure we don't go off the edge!
        newDest =
            Pos
                (clamp (0 + buffer) (1600 - buffer) destX)
                (clamp (0 + buffer) (900 - buffer) destY)

        newNode =
            { node | dest = newDest }
    in
    Dict.insert nodeId newNode nodes


nodeInBoxFilterMap : Pos -> Pos -> Node -> Maybe Node.Id
nodeInBoxFilterMap pos1 pos2 node =
    let
        nodeX =
            node.pos.x

        nodeY =
            node.pos.y

        boxX1 =
            min pos1.x pos2.x

        boxX2 =
            max pos1.x pos2.x

        boxY1 =
            min pos1.y pos2.y

        boxY2 =
            max pos1.y pos2.y
    in
    if (boxX1 <= nodeX) && (nodeX <= boxX2) && (boxY1 <= nodeY) && (nodeY <= boxY2) then
        Just node.id

    else
        Nothing


getTopTouchingNodeId : Config -> Pos -> Dict Node.Id Node -> Maybe Node.Id
getTopTouchingNodeId config mousePos nodes =
    let
        nodeList =
            Dict.values nodes
    in
    getTopTouchingNodeId_ config mousePos nodeList Nothing


getTopTouchingNodeId_ : Config -> Pos -> List Node -> Maybe Node.Id -> Maybe Node.Id
getTopTouchingNodeId_ config mousePos nodes foundId =
    case foundId of
        Just nodeId ->
            -- short circuit
            Just nodeId

        Nothing ->
            case List.head nodes of
                Just node ->
                    if isTouching config mousePos node then
                        Just node.id

                    else
                        case List.tail nodes of
                            Just restOfNodes ->
                                getTopTouchingNodeId_ config mousePos restOfNodes Nothing

                            Nothing ->
                                Nothing

                Nothing ->
                    Nothing


updateMouseDown : Model -> MousePos -> ( Model, Cmd Msg )
updateMouseDown model newMousePos =
    case model.appState of
        AppState.Active gameState ->
            let
                newPos =
                    mousePosToPos newMousePos

                topTouchingNodeId =
                    getTopTouchingNodeId model.config newPos gameState.nodes

                newMouseState =
                    case topTouchingNodeId of
                        Just draggedId ->
                            case gameState.mouseState of
                                MouseState.Lassoed nodeIds ->
                                    if List.member draggedId nodeIds then
                                        let
                                            nodeOffsetList =
                                                List.map (nodeIdToNodeOffset newPos gameState.nodes) nodeIds
                                        in
                                        MouseState.DraggingLassoed nodeOffsetList

                                    else
                                        let
                                            draggedNode =
                                                getNode gameState.nodes draggedId

                                            dragOffset =
                                                Pos (draggedNode.pos.x - newPos.x) (draggedNode.pos.y - newPos.y)

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
                                                List.filterMap (getNeighborNodeIfMatchingEdge draggedNode.id) gameState.edges
                                        in
                                        MouseState.Dragging draggedId dragOffset neighboringNodeIds

                                _ ->
                                    let
                                        draggedNode =
                                            getNode gameState.nodes draggedId

                                        dragOffset =
                                            Pos (draggedNode.pos.x - newPos.x) (draggedNode.pos.y - newPos.y)

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
                                            List.filterMap (getNeighborNodeIfMatchingEdge draggedNode.id) gameState.edges
                                    in
                                    MouseState.Dragging draggedId dragOffset neighboringNodeIds

                        Nothing ->
                            MouseState.Lassoing newPos newPos []

                newGameState =
                    { gameState | mouseState = newMouseState }

                newModel =
                    { model | appState = AppState.Active newGameState }
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


nodeIdToNodeOffset : Pos -> Dict Node.Id Node -> Node.Id -> ( Node.Id, Pos )
nodeIdToNodeOffset mousePos nodes nodeId =
    let
        node =
            getNode nodes nodeId

        offset =
            Pos (node.pos.x - mousePos.x) (node.pos.y - mousePos.y)
    in
    ( nodeId, offset )


updateMouseUp : Model -> MousePos -> ( Model, Cmd Msg )
updateMouseUp model mousePos =
    case model.appState of
        AppState.Active gameState ->
            let
                newMouseState =
                    case gameState.mouseState of
                        MouseState.Lassoing _ _ nodeIds ->
                            MouseState.Lassoed nodeIds

                        _ ->
                            MouseState.Default

                newGameState =
                    { gameState | mouseState = newMouseState }

                newModel =
                    { model | appState = AppState.Active newGameState }

                -- apply any new hover effects
                ( newNewModel, _ ) =
                    updateMouseMove newModel mousePos
            in
            ( newNewModel
            , checkForIntersections
                ( Dict.values newGameState.nodes, newGameState.edges, gameState.difficulty )
            )

        _ ->
            ( model, Cmd.none )


updateAnimation : Model -> Float -> ( Model, Cmd Msg )
updateAnimation model time =
    case model.appState of
        AppState.Active gameState ->
            let
                newGameState =
                    tick time gameState

                newModel =
                    { model | appState = AppState.Active newGameState }
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


makeNode : Config -> Int -> Int -> Node.Id -> ( Node.Id, Node )
makeNode config maxNodes i nodeId =
    --let
    --rotation =
    --    toFloat i / toFloat maxNodes
    --x =
    --    Cfg.graphCenterX + cos (2 * pi * rotation) * Cfg.graphRadius
    --y =
    --    Cfg.graphCenterY + sin (2 * pi * rotation) * Cfg.graphRadius
    --in
    ( nodeId
    , { id = nodeId
      , dest = Pos Cfg.graphCenterX Cfg.graphCenterY
      , pos = Pos Cfg.graphCenterX Cfg.graphCenterY
      , vel = Vel 0 0 0 0
      }
    )


tick : Float -> ActiveStateData -> ActiveStateData
tick timeElapsed ({ nodes, mode } as activeStateData) =
    case mode of
        GameMode.Loading age ->
            { activeStateData
                | nodes =
                    nodes
                        |> Dict.map (moveNodeForLoadAnim age (Dict.size nodes))
                        |> Dict.map (animateNode timeElapsed)
                , mode =
                    if age < Cfg.loadAnimDur then
                        GameMode.Loading (age + timeElapsed)

                    else
                        GameMode.Playing
            }

        GameMode.Playing ->
            { activeStateData
                | nodes =
                    nodes
                        |> Dict.map (animateNode timeElapsed)
            }

        GameMode.Won time shapes ->
            { activeStateData
                | nodes = nodes |> Dict.map (animateNode timeElapsed)
                , mode =
                    --TODO maybe shimmer here?
                    GameMode.Won (time + timeElapsed) shapes
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

        rotation =
            (toFloat id / toFloat numNodes) + (easeRot * 0.1)

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


animateNode : Float -> Node.Id -> Node -> Node
animateNode timeElapsed _ node =
    let
        newNode =
            moveNodeToDest node timeElapsed
    in
    newNode


moveNodeToDest : Node -> Float -> Node
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


isTouching : Config -> Pos -> Node -> Bool
isTouching config mousePos node =
    let
        aSquared =
            (node.pos.x - mousePos.x) ^ 2

        bSquared =
            (node.pos.y - mousePos.y) ^ 2

        c =
            sqrt (aSquared + bSquared)
    in
    c < config.radius


calculatePosition : Node -> Float -> Pos
calculatePosition { dest, pos } timeElapsed =
    let
        dragSpeed =
            1 - (baseWeight / (baseWeight + timeElapsed))

        distX =
            dest.x - pos.x

        distY =
            dest.y - pos.y

        newX =
            pos.x + (dragSpeed * distX)

        newY =
            pos.y + (dragSpeed * distY)
    in
    Pos newX newY


calculateVel : Node -> Pos -> Float -> Vel
calculateVel { pos } newPos timeElapsed =
    let
        xDiff =
            (newPos.x - pos.x) / timeElapsed

        yDiff =
            (newPos.y - pos.y) / timeElapsed

        ( r, a ) =
            toPolar ( xDiff, yDiff )
    in
    Vel xDiff yDiff r a


mousePosToPos : MousePos -> Pos
mousePosToPos ( x, y ) =
    Pos
        (1600 * x)
        (900 * y)


updateGetIntersectionResults : Model -> IntersectionResultData -> ( Model, Cmd Msg )
updateGetIntersectionResults ({ appState } as model) intersectionResultData =
    case appState of
        AppState.Active gameState ->
            let
                isIntersecting =
                    Tuple.first intersectionResultData

                newEdges =
                    Tuple.second intersectionResultData

                newGameState =
                    { gameState
                        | edges = newEdges
                        , mode =
                            if isIntersecting then
                                GameMode.Playing

                            else
                                GameMode.Won 0 (getShapes gameState.nodes newEdges)
                    }

                newModel =
                    { model
                        | appState = AppState.Active newGameState
                        , levelsCleared =
                            if isIntersecting then
                                model.levelsCleared

                            else
                                max model.levelsCleared gameState.difficulty
                    }

                _ =
                    if isIntersecting then
                        newModel

                    else
                        Debug.log "model" newModel
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


getShapes : Dict Node.Id Node -> List Edge -> List Shape
getShapes nodes edges =
    edges
        |> List.map
            (\{ pair } ->
                let
                    ( id1, id2 ) =
                        pair

                    node1 =
                        getNode nodes id1

                    node2 =
                        getNode nodes id2
                in
                [ getShapeNodeIdsForRay nodes edges ( node1, node2 )
                , getShapeNodeIdsForRay nodes edges ( node2, node1 )
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
                            getNode nodes nodeId
                                |> (\{ pos } ->
                                        pos.y - pos.x
                                   )
                        )
                    |> Maybe.withDefault -1
                    |> getNode nodes
                    |> (\{ pos } ->
                            pos.y - pos.x
                       )
            )
        |> List.indexedMap
            (\i nodeIds ->
                { pts =
                    nodeIds
                        |> List.map (getNode nodes)
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


getRandomColor : Int -> String
getRandomColor seed =
    Random.step Colors.starryNightColorGen (Random.initialSeed seed)
        |> Tuple.first


getShapeNodeIdsForRay : Dict Node.Id Node -> List Edge -> ( Node, Node ) -> List Node.Id
getShapeNodeIdsForRay nodes edges (( node1, node2 ) as ray) =
    getShapeForRayHelper nodes edges node1 [ node2, node1 ] ray
        |> List.map .id


getShapeForRayHelper : Dict Node.Id Node -> List Edge -> Node -> List Node -> ( Node, Node ) -> List Node
getShapeForRayHelper nodes edges genesisNode shapeNodes ( node1, node2 ) =
    -- get neighbors of node2 (but not node1)
    getNeighborsOfNode nodes edges node2.id
        |> List.filter (\n -> n /= node1)
        -- then find the ccw-most node of node2
        |> List.Extra.minimumBy (\n -> angleFor3Pts node1.pos node2.pos n.pos)
        |> Maybe.withDefault nothingNode
        |> (\nextNode ->
                if nextNode == genesisNode then
                    shapeNodes

                else
                    getShapeForRayHelper nodes edges genesisNode (nextNode :: shapeNodes) ( node2, nextNode )
           )


getNeighborsOfNode : Dict Node.Id Node -> List Edge -> Node.Id -> List Node
getNeighborsOfNode nodes edges nodeId =
    edges
        |> List.filterMap
            (\{ pair } ->
                let
                    ( node1, node2 ) =
                        pair
                in
                if node1 == nodeId then
                    Just (getNode nodes node2)

                else if node2 == nodeId then
                    Just (getNode nodes node1)

                else
                    Nothing
            )


closingShapePoints : Node -> Node -> Dict Node.Id Node -> List Edge -> Node.Id -> List Pos
closingShapePoints genesisNode prevNode nodes edges curNodeId =
    let
        nextNode =
            getCcwNode genesisNode prevNode curNodeId nodes edges

        --|> Debug.log "nextNode"
        --_ =
        --    Debug.log "genesisNode, prevNode, curNodeId" ( genesisNode.id, prevNode.id, curNodeId )
    in
    if nextNode.id == genesisNode.id then
        [ nextNode.pos ]

    else
        nextNode.pos :: closingShapePoints prevNode genesisNode nodes edges nextNode.id


getCcwNode : Node -> Node -> Node.Id -> Dict Node.Id Node -> List Edge -> Node
getCcwNode genesisNode prevNode nodeId nodes edges =
    let
        node =
            getNode nodes nodeId
    in
    edges
        |> List.filterMap
            (\{ pair } ->
                let
                    ( nodeId1, nodeId2 ) =
                        pair
                in
                if nodeId1 == nodeId then
                    Just nodeId2

                else if nodeId2 == nodeId then
                    Just nodeId1

                else
                    Nothing
            )
        |> List.map (getNode nodes)
        |> List.Extra.minimumBy
            (\n ->
                angleFor3Pts prevNode.pos node.pos n.pos
            )
        |> Maybe.withDefault nothingNode


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


getNode : Dict Node.Id Node -> Node.Id -> Node
getNode nodes nodeId =
    case Dict.get nodeId nodes of
        Just node ->
            node

        Nothing ->
            -- should never happen
            nothingNode


nothingNode : Node
nothingNode =
    -- should never happen
    { id = -1
    , dest = Pos 42 42
    , pos = Pos 42 42
    , vel = Vel 0 0 0 0
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loadedLevelFresh LoadedLevelFresh
        , loadedLevelInProgress LoadedLevelInProgress
        , intersectionResults GetIntersectionResults
        , Browser.Events.onAnimationFrameDelta AnimationMsg
        ]
