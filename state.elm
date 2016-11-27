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


init : ( Model, Cmd Msg )
init =
    let
        appState =
            -- FIXME
            LoadingState -1

        config =
            { radius = 25
            }
    in
        ( { appState = appState
          , config = config
          }
        , generateEdges baseDifficulty
        )



-- UPDATE


port generateEdges : Int -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newAppState, cmd ) =
            case model.appState of
                LoadingState difficulty ->
                    updateFromLoadingState model.config msg difficulty

                ActiveState gameState ->
                    updateFromGameState model.config msg gameState
    in
        ( { model | appState = newAppState }, cmd )


updateFromLoadingState : Config -> Msg -> Int -> ( AppState, Cmd Msg )
updateFromLoadingState config msg difficulty =
    let
        loadingState =
            LoadingState difficulty
    in
        case msg of
            MouseMove mousePos ->
                ( loadingState, Cmd.none )

            MouseDown mousePos ->
                ( loadingState, Cmd.none )

            MouseUp mousePos ->
                ( loadingState, Cmd.none )

            AnimationMsg time ->
                ( loadingState, Cmd.none )

            -- request to js-land
            GenerateEdges newDifficulty ->
                ( loadingState, generateEdges newDifficulty )

            -- response from js-land
            GeneratedEdges edgeData ->
                ( ActiveState (edgeDataToGameData config edgeData), Cmd.none )


edgeDataToGameData : Config -> EdgeData -> GameState
edgeDataToGameData config edgeData =
    let
        ( edges, numNodes ) =
            edgeData

        nodeIdList =
            List.range 0 (numNodes - 1)

        nodes =
            List.map (makeNode config numNodes) nodeIdList

        newGameState =
            ({ nodes = Dict.fromList nodes
             , edges = edges
             , mouse = { pos = (Pos 0 0) }
             , now = 0
             , difficulty =
                -1
                -- FIXME
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
          , rad = config.radius
          , dest = (Pos x y)
          , pos = (Pos x y)
          , vel = (Vel 0 0 0 0)
          , isHovered = False
          , dragOffset = Nothing
          }
        )


updateFromGameState : Config -> Msg -> GameState -> ( AppState, Cmd Msg )
updateFromGameState config msg gameState =
    let
        mouse =
            gameState.mouse

        nodes =
            gameState.nodes
    in
        case msg of
            MouseMove mousePos ->
                let
                    newPos =
                        mousePosToPos mousePos

                    newMouse =
                        { mouse | pos = newPos }

                    newGameState =
                        { gameState | mouse = newMouse }
                in
                    ( ActiveState newGameState, Cmd.none )

            MouseDown mousePos ->
                let
                    newPos =
                        mousePosToPos mousePos

                    newMouse =
                        { mouse | pos = newPos }

                    processMouseDown pos _ node =
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
                        Dict.map (processMouseDown newPos) nodes

                    newGameState =
                        { gameState
                            | mouse = newMouse
                            , nodes = newNodes
                        }
                in
                    ( ActiveState newGameState, Cmd.none )

            MouseUp mousePos ->
                let
                    newPos =
                        mousePosToPos mousePos

                    newMouse =
                        { mouse
                            | pos = newPos
                        }

                    processMouseUp pos _ node =
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
                        Dict.map (processMouseUp newPos) nodes

                    newGameState =
                        { gameState
                            | nodes = newNodes
                            , mouse = newMouse
                        }
                in
                    ( ActiveState newGameState, Cmd.none )

            -- request to js-land
            GenerateEdges difficulty ->
                ( ActiveState gameState, generateEdges gameState.difficulty )

            -- response from js-land
            GeneratedEdges edgeData ->
                ( ActiveState (edgeDataToGameData config edgeData), Cmd.none )

            AnimationMsg time ->
                ( ActiveState (animate time gameState), Cmd.none )


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
