port module State exposing (init, update, subscriptions)

import AnimationFrame
import Dict
import Mouse
import Time


-- mine

import Types exposing (..)


baseDifficulty =
    1


baseWeight =
    50


init : Config -> ( Model, Cmd Msg )
init config =
    let
        appState =
            LoadingState
    in
        ( { appState = appState
          , config = config
          }
        , generateEdges baseDifficulty
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


updateMouseMove : Model -> Mouse.Position -> ( Model, Cmd Msg )
updateMouseMove model newMousePos =
    case model.appState of
        LoadingState ->
            ( model, Cmd.none )

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

                newModel =
                    { model | appState = ActiveState newGameState }
            in
                ( newModel, Cmd.none )


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
        if ((boxX1 <= nodeX) && (nodeX <= boxX2) && (boxY1 <= nodeY) && (nodeY <= boxY2)) then
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


updateMouseDown : Model -> Mouse.Position -> ( Model, Cmd Msg )
updateMouseDown model newMousePos =
    case model.appState of
        LoadingState ->
            ( model, Cmd.none )

        ActiveState gameState ->
            let
                newPos =
                    mousePosToPos newMousePos

                topTouchingNodeId =
                    getTopTouchingNodeId model.config newPos gameState.nodes

                newMouseState =
                    case topTouchingNodeId of
                        Just draggedId ->
                            let
                                draggedNode =
                                    getNode gameState.nodes draggedId

                                dragOffset =
                                    (Pos (draggedNode.pos.x - newPos.x) (draggedNode.pos.y - newPos.y))

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


updateMouseUp : Model -> Mouse.Position -> ( Model, Cmd Msg )
updateMouseUp model mousePos =
    case model.appState of
        LoadingState ->
            ( model, Cmd.none )

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


updateAnimation : Model -> Time.Time -> ( Model, Cmd Msg )
updateAnimation model time =
    case model.appState of
        LoadingState ->
            ( model, Cmd.none )

        ActiveState gameState ->
            let
                newGameState =
                    animate time gameState

                newModel =
                    { model | appState = ActiveState newGameState }
            in
                ( newModel, Cmd.none )


updateGenerateEdges : Model -> Int -> ( Model, Cmd Msg )
updateGenerateEdges model difficulty =
    let
        newModel =
            { model | appState = LoadingState }
    in
        ( newModel, generateEdges difficulty )


updateGeneratedEdges : Model -> EdgeData -> ( Model, Cmd Msg )
updateGeneratedEdges model edgeData =
    let
        newGameState =
            edgeDataToGameData model.config edgeData

        newModel =
            { model | appState = ActiveState newGameState }
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
            ({ nodes = Dict.fromList nodes
             , edges = edges
             , difficulty = difficulty
             , mouseState = DefaultMouseState
             , hasWon = False
             }
            )
    in
        newGameState


makeNode : Config -> Int -> NodeId -> ( NodeId, Node )
makeNode config maxNodes nodeId =
    let
        graphCenterX =
            500

        graphCenterY =
            250

        graphRadius =
            200

        rotation =
            toFloat nodeId / (toFloat maxNodes)

        x =
            graphCenterX + cos (2 * pi * rotation) * graphRadius

        y =
            graphCenterY + sin (2 * pi * rotation) * graphRadius
    in
        ( nodeId
        , { id = nodeId
          , dest = (Pos x y)
          , pos = (Pos x y)
          , vel = (Vel 0 0 0 0)
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


mousePosToPos : Mouse.Position -> Pos
mousePosToPos mousePos =
    Pos (toFloat mousePos.x) (toFloat mousePos.y)


updateGetIntersectionResults : Model -> IntersectionResultData -> ( Model, Cmd Msg )
updateGetIntersectionResults model intersectionResultData =
    case model.appState of
        LoadingState ->
            ( model, Cmd.none )

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
