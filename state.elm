port module State exposing (init, update, subscriptions)

import AnimationFrame
import Dict
import Mouse
import Time
import Set


-- mine

import Types exposing (..)


baseDifficulty =
    6


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


updateMouseMove : Model -> Mouse.Position -> ( Model, Cmd Msg )
updateMouseMove model newMousePos =
    case model.appState of
        LoadingState ->
            ( model, Cmd.none )

        ActiveState gameState ->
            let
                newPos =
                    mousePosToPos newMousePos

                mouse =
                    gameState.mouse

                newMouse =
                    { mouse | pos = newPos }

                topTouchingNodeId =
                    getTopTouchingNodeId model.config newPos gameState.nodes

                updateNodeState topTouchingNodeId _ node =
                    case topTouchingNodeId of
                        Just id ->
                            if node.id == id then
                                { node | state = Hovered }
                            else
                                { node | state = Default }

                        Nothing ->
                            { node | state = Default }

                newNodes =
                    Dict.map (updateNodeState topTouchingNodeId) gameState.nodes

                newGameState =
                    { gameState
                        | mouse = newMouse
                        , nodes = newNodes
                    }

                newModel =
                    { model | appState = ActiveState newGameState }
            in
                ( newModel, Cmd.none )


getTopTouchingNodeId : Config -> Pos -> Dict.Dict Id Node -> Maybe Id
getTopTouchingNodeId config mousePos nodes =
    let
        nodeList =
            Dict.values nodes
    in
        getTopTouchingNodeId_ config mousePos nodeList Nothing


getTopTouchingNodeId_ : Config -> Pos -> List Node -> Maybe Id -> Maybe Id
getTopTouchingNodeId_ config mousePos nodes foundId =
    case foundId of
        Just id ->
            -- short circuit
            Just id

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
                mouse =
                    gameState.mouse

                newPos =
                    mousePosToPos newMousePos

                newMouse =
                    { mouse | pos = newPos }

                processMouseDown pos _ node =
                    node

                --let
                --    ( dragOffset, newDest ) =
                --        if isTouching model.config pos node then
                --            ( Just (Pos (node.pos.x - newPos.x) (node.pos.y - newPos.y))
                --            , pos
                --            )
                --        else
                --            ( Nothing, node.dest )
                --in
                --    { node
                --        | state = Dragged dragOffset
                --        , dest = newDest
                --    }
                newNodes =
                    Dict.map (processMouseDown newPos) gameState.nodes

                newGameState =
                    { gameState
                        | mouse = newMouse
                        , nodes = newNodes
                    }

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
                mouse =
                    gameState.mouse

                newPos =
                    mousePosToPos mousePos

                newMouse =
                    { mouse
                        | pos = newPos
                    }

                processMouseUp pos _ node =
                    node

                --let
                --    newDest =
                --        case node.dragOffset of
                --            Just dragOffset ->
                --                Pos (pos.x + dragOffset.x) (pos.y + dragOffset.y)
                --            Nothing ->
                --                node.dest
                --in
                --    { node
                --        | dragOffset = Nothing
                --        , dest = newDest
                --    }
                newNodes =
                    Dict.map (processMouseUp newPos) gameState.nodes

                newGameState =
                    { gameState
                        | nodes = newNodes
                        , mouse = newMouse
                    }

                newModel =
                    { model | appState = ActiveState newGameState }
            in
                ( newModel, Cmd.none )


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
        ( newModel, Cmd.none )


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
             , mouse = { pos = (Pos 0 0) }
             , difficulty = difficulty
             }
            )
    in
        newGameState


makeNode : Config -> Int -> Id -> ( Id, Node )
makeNode config maxNodes id =
    let
        graphCenterX =
            500

        graphCenterY =
            250

        graphRadius =
            200

        rotation =
            toFloat id / (toFloat maxNodes - 1)

        x =
            graphCenterX + cos (2 * pi * rotation) * graphRadius

        y =
            graphCenterY + sin (2 * pi * rotation) * graphRadius
    in
        ( id
        , { id = id
          , dest = (Pos x y)
          , pos = (Pos x y)
          , vel = (Vel 0 0 0 0)
          , isHovered = False
          , state = Default
          }
        )


animate : Time.Time -> GameState -> GameState
animate timeElapsed gameState =
    let
        nodes =
            gameState.nodes

        mouse =
            gameState.mouse

        newNodes =
            Dict.map (animateNode timeElapsed mouse.pos) nodes

        -- TODO isHovered
    in
        { gameState
            | nodes = newNodes
        }


animateNode : Time.Time -> Pos -> Id -> Node -> Node
animateNode timeElapsed mousePos _ node =
    let
        newDest =
            case node.state of
                Default ->
                    node.dest

                Hovered ->
                    node.dest

                Dragged offset ->
                    let
                        destX =
                            mousePos.x + offset.x

                        destY =
                            mousePos.y + offset.y
                    in
                        Pos destX destY

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



-- SUBSCRIPTIONS


port generatedEdges : (EdgeData -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ generatedEdges GeneratedEdges
        , AnimationFrame.diffs AnimationMsg
        ]
