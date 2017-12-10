port module State exposing (init, subscriptions, update)

-- mine

import AnimationFrame
import Dict
import Ease
import Mouse
import Navigation
import Time exposing (Time)
import Types exposing (..)


baseDifficulty =
    1


baseWeight =
    50


init : Config -> Navigation.Location -> ( Model, Cmd Msg )
init config location =
    case location.hash of
        "#load" ->
            ( { appState = StartState --LoadingState 0 12
              , config = config
              }
            , generateEdges 5
            )

        _ ->
            ( { appState = StartState
              , config = config
              }
            , Cmd.none
            )



-- UPDATE


port generateEdges : Int -> Cmd msg


port saveConfig : Config -> Cmd msg


port checkForIntersections : ( List Node, List Edge ) -> Cmd msg


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

        -- request to js-land
        GenerateEdges newDifficulty ->
            updateGenerateEdges model newDifficulty

        -- response from js-land
        GeneratedEdges edgeData ->
            updateGeneratedEdges model edgeData ! []

        ChangeConfigRadius newRadius ->
            updateConfigRadius model newRadius

        GetIntersectionResults intersectionResultData ->
            updateGetIntersectionResults model intersectionResultData

        StartCampaign ->
            ( model, generateEdges 1 )

        UrlChange location ->
            ( model, Cmd.none )


updateMouseMove : Model -> MousePos -> ( Model, Cmd Msg )
updateMouseMove model newMousePos =
    case model.appState of
        ActiveState ({ mouseState, nodes } as gameState) ->
            let
                newPos =
                    mousePosToPos newMousePos

                newGameState =
                    case mouseState of
                        DefaultMouseState ->
                            let
                                topTouchingNodeId =
                                    getTopTouchingNodeId model.config newPos nodes

                                newMouseState =
                                    case topTouchingNodeId of
                                        Just nodeId ->
                                            HoveringMouseState nodeId

                                        Nothing ->
                                            DefaultMouseState
                            in
                            { gameState | mouseState = newMouseState }

                        HoveringMouseState _ ->
                            let
                                topTouchingNodeId =
                                    getTopTouchingNodeId model.config newPos nodes

                                newMouseState =
                                    case topTouchingNodeId of
                                        Just nodeId ->
                                            HoveringMouseState nodeId

                                        Nothing ->
                                            DefaultMouseState
                            in
                            { gameState | mouseState = newMouseState }

                        DraggingMouseState nodeId offset neighboringNodeIds ->
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

                        LassoingMouseState startPos _ nodeIds ->
                            let
                                lassoedNodes =
                                    List.filterMap (nodeInBoxFilterMap startPos newPos) (Dict.values nodes)

                                newMouseState =
                                    LassoingMouseState startPos newPos lassoedNodes
                            in
                            { gameState | mouseState = newMouseState }

                        LassoedMouseState nodeIds ->
                            gameState

                        DraggingLassoedMouseState nodeOffsetList ->
                            let
                                newNodes =
                                    List.foldr (moveNodeOffset newPos) nodes nodeOffsetList
                            in
                            { gameState | nodes = newNodes }

                newModel =
                    { model | appState = ActiveState newGameState }
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


moveNodeOffset : Pos -> ( NodeId, Pos ) -> Dict.Dict NodeId Node -> Dict.Dict NodeId Node
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


nodeInBoxFilterMap : Pos -> Pos -> Node -> Maybe NodeId
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


getTopTouchingNodeId : Config -> Pos -> Dict.Dict NodeId Node -> Maybe NodeId
getTopTouchingNodeId config mousePos nodes =
    let
        nodeList =
            Dict.values nodes
    in
    getTopTouchingNodeId_ config mousePos nodeList Nothing


getTopTouchingNodeId_ : Config -> Pos -> List Node -> Maybe NodeId -> Maybe NodeId
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
        ActiveState gameState ->
            let
                newPos =
                    mousePosToPos newMousePos

                topTouchingNodeId =
                    getTopTouchingNodeId model.config newPos gameState.nodes

                newMouseState =
                    case topTouchingNodeId of
                        Just draggedId ->
                            case gameState.mouseState of
                                LassoedMouseState nodeIds ->
                                    if List.member draggedId nodeIds then
                                        let
                                            nodeOffsetList =
                                                List.map (nodeIdToNodeOffset newPos gameState.nodes) nodeIds
                                        in
                                        DraggingLassoedMouseState nodeOffsetList
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
                                        DraggingMouseState draggedId dragOffset neighboringNodeIds

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
                                    DraggingMouseState draggedId dragOffset neighboringNodeIds

                        Nothing ->
                            LassoingMouseState newPos newPos []

                newGameState =
                    { gameState | mouseState = newMouseState }

                newModel =
                    { model | appState = ActiveState newGameState }
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


nodeIdToNodeOffset : Pos -> Dict.Dict NodeId Node -> NodeId -> ( NodeId, Pos )
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
        ActiveState gameState ->
            let
                newMouseState =
                    case gameState.mouseState of
                        LassoingMouseState _ _ nodeIds ->
                            LassoedMouseState nodeIds

                        _ ->
                            DefaultMouseState

                newGameState =
                    { gameState | mouseState = newMouseState }

                newModel =
                    { model | appState = ActiveState newGameState }

                -- apply any new hover effects
                ( newNewModel, _ ) =
                    updateMouseMove newModel mousePos
            in
            ( newNewModel, checkForIntersections ( Dict.values newGameState.nodes, newGameState.edges ) )

        _ ->
            ( model, Cmd.none )


updateAnimation : Model -> Time -> ( Model, Cmd Msg )
updateAnimation model time =
    case model.appState of
        ActiveState gameState ->
            let
                newGameState =
                    tick time gameState

                newModel =
                    { model | appState = ActiveState newGameState }
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateGenerateEdges : Model -> Int -> ( Model, Cmd Msg )
updateGenerateEdges model difficulty =
    ( model, generateEdges difficulty )


updateGeneratedEdges : Model -> EdgeData -> Model
updateGeneratedEdges model edgeData =
    { model | appState = ActiveState (edgeDataToActiveStateData model.config edgeData) }


updateConfigRadius : Model -> String -> ( Model, Cmd Msg )
updateConfigRadius model radiusString =
    let
        config =
            model.config

        radiusFloat =
            Result.withDefault
                config.radius
                (String.toFloat radiusString)

        newConfig =
            { config | radius = radiusFloat }

        newModel =
            { model | config = newConfig }
    in
    ( newModel, saveConfig newConfig )


edgeDataToActiveStateData : Config -> EdgeData -> ActiveStateData
edgeDataToActiveStateData config ( edges, numNodes, difficulty ) =
    let
        nodes =
            List.range 0 (numNodes - 1)
                |> List.map (makeNode config numNodes)

        newGameState =
            { nodes = Dict.fromList nodes
            , edges = edges
            , difficulty = difficulty
            , mouseState = DefaultMouseState
            , mode = LoadingMode 0
            }
    in
    newGameState


makeNode : Config -> Int -> NodeId -> ( NodeId, Node )
makeNode config maxNodes nodeId =
    let
        rotation =
            toFloat nodeId / toFloat maxNodes

        x =
            graphCenterX + cos (2 * pi * rotation) * graphRadius

        y =
            graphCenterY + sin (2 * pi * rotation) * graphRadius
    in
    ( nodeId
    , { id = nodeId
      , dest = Pos graphCenterX graphCenterY
      , pos = Pos graphCenterX graphCenterY
      , vel = Vel 0 0 0 0
      }
    )


tick : Time -> ActiveStateData -> ActiveStateData
tick timeElapsed ({ nodes, mode } as activeStateData) =
    case mode of
        LoadingMode age ->
            { activeStateData
                | nodes =
                    nodes
                        |> Dict.map (moveNodeForLoadAnim age (Dict.size nodes))
                        |> Dict.map (animateNode timeElapsed)
                , mode =
                    if age < loadAnimDur then
                        LoadingMode (age + timeElapsed)
                    else
                        PlayingMode { hasWon = False }
            }

        PlayingMode _ ->
            { activeStateData
                | nodes =
                    nodes
                        |> Dict.map (animateNode timeElapsed)
            }


moveNodeForLoadAnim : Time -> Int -> NodeId -> Node -> Node
moveNodeForLoadAnim time numNodes id node =
    let
        age =
            if time < wait then
                0
            else
                min loadAnimDur (time - wait)

        ease =
            Ease.outElastic (age / loadAnimDur)

        easeRot =
            Ease.outCubic (age / loadAnimDur)

        easeInv =
            1 - ease

        rotation =
            (toFloat id / toFloat numNodes) + (easeRot * 0.1)

        destX =
            graphCenterX + cos (2 * pi * rotation) * graphRadius

        destY =
            graphCenterY + sin (2 * pi * rotation) * graphRadius
    in
    { node
        | dest =
            Pos (ease * destX + easeInv * graphCenterX)
                (ease * destY + easeInv * graphCenterY)
    }


animateNode : Time -> NodeId -> Node -> Node
animateNode timeElapsed _ node =
    let
        newNode =
            moveNodeToDest node timeElapsed
    in
    newNode


moveNodeToDest : Node -> Time -> Node
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


calculatePosition : Node -> Time -> Pos
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


calculateVel : Node -> Pos -> Time -> Vel
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
updateGetIntersectionResults model intersectionResultData =
    case model.appState of
        ActiveState gameState ->
            let
                isIntersecting =
                    Tuple.first intersectionResultData

                newEdges =
                    Tuple.second intersectionResultData

                newGameState =
                    { gameState
                        | edges = newEdges
                        , mode = PlayingMode { hasWon = not isIntersecting }
                    }

                newModel =
                    { model | appState = ActiveState newGameState }
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


port generatedEdges : (EdgeData -> msg) -> Sub msg


port intersectionResults : (( Bool, List Edge ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ generatedEdges GeneratedEdges
        , intersectionResults GetIntersectionResults
        , AnimationFrame.diffs AnimationMsg
        ]
