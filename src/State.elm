port module State exposing (init, subscriptions, update)

-- mine

import AnimationFrame
import Dict
import Mouse
import Navigation
import Time
import Types exposing (..)


baseDifficulty =
    1


baseWeight =
    50


init : Config -> Navigation.Location -> ( Model, Cmd Msg )
init config location =
    case location.hash of
        "#load" ->
            ( { appState = LoadingState
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
            updateGeneratedEdges model edgeData

        ChangeConfigRadius newRadius ->
            updateConfigRadius model newRadius

        GetIntersectionResults intersectionResultData ->
            updateGetIntersectionResults model intersectionResultData

        StartCampaign ->
            ( { model | appState = LoadingCampaignState }
            , generateEdges 1
              -- TODO load save
            )

        CloseNarration ->
            case model.appState of
                ActiveState gameState ->
                    let
                        newGameState =
                            { gameState | isNarrationVisible = False }

                        newModel =
                            { model | appState = ActiveState newGameState }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UrlChange location ->
            ( model, Cmd.none )


updateMouseMove : Model -> MousePos -> ( Model, Cmd Msg )
updateMouseMove model newMousePos =
    case model.appState of
        ActiveState gameState ->
            let
                newPos =
                    mousePosToPos newMousePos

                newGameState =
                    case gameState.mouseState of
                        DefaultMouseState ->
                            let
                                topTouchingNodeId =
                                    getTopTouchingNodeId model.config newPos gameState.nodes

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
                                    getTopTouchingNodeId model.config newPos gameState.nodes

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
                                    getNode gameState.nodes nodeId

                                destX =
                                    newPos.x + offset.x

                                destY =
                                    newPos.y + offset.y

                                newDest =
                                    Pos destX destY

                                newDraggedNode =
                                    { draggedNode | dest = newDest }

                                newNodes =
                                    Dict.insert nodeId newDraggedNode gameState.nodes
                            in
                            { gameState | nodes = newNodes }

                        LassoingMouseState startPos _ nodeIds ->
                            let
                                lassoedNodes =
                                    List.filterMap (nodeInBoxFilterMap startPos newPos) (Dict.values gameState.nodes)

                                newMouseState =
                                    LassoingMouseState startPos newPos lassoedNodes
                            in
                            { gameState | mouseState = newMouseState }

                        LassoedMouseState nodeIds ->
                            gameState

                        DraggingLassoedMouseState nodeOffsetList ->
                            let
                                newNodes =
                                    List.foldr (moveNodeOffset newPos) gameState.nodes nodeOffsetList
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


updateAnimation : Model -> Time.Time -> ( Model, Cmd Msg )
updateAnimation model time =
    case model.appState of
        ActiveState gameState ->
            let
                newGameState =
                    animate time gameState

                newModel =
                    { model | appState = ActiveState newGameState }
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateGenerateEdges : Model -> Int -> ( Model, Cmd Msg )
updateGenerateEdges model difficulty =
    let
        newAppState =
            case model.appState of
                ActiveState gameState ->
                    if gameState.isSandbox then
                        LoadingState
                    else
                        LoadingCampaignState

                _ ->
                    model.appState

        newModel =
            { model | appState = newAppState }
    in
    ( newModel, generateEdges difficulty )


updateGeneratedEdges : Model -> EdgeData -> ( Model, Cmd Msg )
updateGeneratedEdges model edgeData =
    let
        newGameState =
            edgeDataToGameData model.config edgeData

        newAppState =
            case model.appState of
                LoadingCampaignState ->
                    ActiveState { newGameState | isSandbox = False }

                ActiveState gameState ->
                    ActiveState { newGameState | isSandbox = gameState.isSandbox }

                _ ->
                    ActiveState { newGameState | isSandbox = True }

        newModel =
            { model | appState = newAppState }
    in
    ( newModel, checkForIntersections ( Dict.values newGameState.nodes, newGameState.edges ) )


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


edgeDataToGameData : Config -> EdgeData -> GameState
edgeDataToGameData config edgeData =
    let
        ( edges, numNodes, difficulty ) =
            edgeData

        nodeIdList =
            List.range 0 (numNodes - 1)

        nodes =
            List.map (makeNode config numNodes) nodeIdList

        newGameState =
            { nodes = Dict.fromList nodes
            , edges = edges
            , difficulty = difficulty
            , mouseState = DefaultMouseState
            , hasWon = False
            , isSandbox = True
            , isNarrationVisible = True
            }
    in
    newGameState


makeNode : Config -> Int -> NodeId -> ( NodeId, Node )
makeNode config maxNodes nodeId =
    let
        graphCenterX =
            800

        graphCenterY =
            450

        graphRadius =
            300

        rotation =
            toFloat nodeId / toFloat maxNodes

        x =
            graphCenterX + cos (2 * pi * rotation) * graphRadius

        y =
            graphCenterY + sin (2 * pi * rotation) * graphRadius
    in
    ( nodeId
    , { id = nodeId
      , dest = Pos x y
      , pos = Pos x y
      , vel = Vel 0 0 0 0
      }
    )


animate : Time.Time -> GameState -> GameState
animate timeElapsed gameState =
    let
        nodes =
            gameState.nodes

        newNodes =
            Dict.map (animateNode timeElapsed) nodes
    in
    { gameState
        | nodes = newNodes
    }


animateNode : Time.Time -> NodeId -> Node -> Node
animateNode timeElapsed _ node =
    let
        newNode =
            moveNodeToDest node timeElapsed
    in
    newNode


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
    Vel xDiff yDiff r a


mousePosToPos : MousePos -> Pos
mousePosToPos mousePos =
    Pos (1600 * Tuple.first mousePos) (900 * Tuple.second mousePos)


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
                        , hasWon = not isIntersecting
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
