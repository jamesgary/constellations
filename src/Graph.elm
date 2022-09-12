module Graph exposing (Graph, animateNodes, applyAspectRatio, checkForIntersections, codec, empty, getEdges, getNode, getNodeIdsInBox, getNodeUnsafe, getNodes, getTouching, init, isEmpty, moveNodesForLoadAnim, neighbors, setNodeDest)

import Array exposing (Array)
import Cfg
import Codec exposing (Codec)
import Dict exposing (Dict)
import Ease
import Edge exposing (Edge)
import List.Extra
import Node exposing (Node)
import Pos exposing (Pos)
import Random
import Random.List
import Set exposing (Set)
import Vel exposing (Vel)



-- TODO use in more places


type Graph
    = Graph Model


type alias Model =
    { nodes : Dict Node.Id Node
    , edges : Dict Edge.Id Edge
    }


init : Int -> Graph
init lvlIndex =
    -- NOTE all poses start at Cfg.graphCenterX,Y
    -- current implementation: create grid
    -- don't remove nodes with 2 or less neighbors (to keep graph intact)
    let
        ( numRows, numCols ) =
            ( 2 + (ceiling <| toFloat lvlIndex / 2)
            , 3 + (floor <| toFloat lvlIndex / 2)
            )

        numNodes =
            numRows * numCols

        edgeList =
            List.range 0 (numRows - 1)
                |> List.map
                    (\row ->
                        List.range 0 (numCols - 1)
                            |> List.map
                                (\col ->
                                    let
                                        nodeId =
                                            (row * numCols) + col

                                        checksAndEdge =
                                            -- connect with top
                                            [ ( row > 0, ( nodeId, nodeId - numCols ) )

                                            -- connect with left
                                            , ( col > 0, ( nodeId, nodeId - 1 ) )

                                            -- connect with top-left
                                            , ( row > 0 && col > 0, ( nodeId, nodeId - 1 - numCols ) )
                                            ]
                                    in
                                    checksAndEdge
                                        |> List.filterMap
                                            (\( check, edge ) ->
                                                if check then
                                                    Just edge

                                                else
                                                    Nothing
                                            )
                                )
                    )
                |> List.concat
                |> List.concat

        randomSeed : Random.Seed
        randomSeed =
            -- only needs to be random once per level
            -- so this hacky solution is fine
            Random.initialSeed (3000000 + lvlIndex)

        shuffledIndices : Array Int
        shuffledIndices =
            Random.step
                (List.range 0 (numNodes - 1)
                    |> Random.List.shuffle
                )
                randomSeed
                |> Tuple.first
                |> Array.fromList

        shuffledEdges : List ( Int, Int )
        shuffledEdges =
            edgeList
                |> List.map
                    (\( n1, n2 ) ->
                        ( Array.get n1 shuffledIndices |> Maybe.withDefault n1
                        , Array.get n2 shuffledIndices |> Maybe.withDefault n2
                        )
                    )

        nodeDict =
            List.range 0 (numNodes - 1)
                |> List.map
                    (\i ->
                        ( String.fromInt i
                        , Node.init
                            (Pos
                                Cfg.graphCenterX
                                Cfg.graphCenterY
                            )
                        )
                    )
                |> Dict.fromList

        edgeDict =
            shuffledEdges
                |> List.map (Tuple.mapBoth String.fromInt String.fromInt)
                |> List.indexedMap (\i edge -> ( String.fromInt i, edge ))
                |> Dict.fromList

        -- remove a couple nodes, just for variety
        -- FIXME small chance of orphan nodes?
        ( newNodes, newEdges ) =
            List.range 0 ((floor <| 0.1 * toFloat numNodes) - 1)
                |> List.foldl
                    (\i ( nodes, edges ) ->
                        let
                            indexToRemove =
                                shuffledIndices
                                    |> Array.get i
                                    |> Maybe.map String.fromInt
                                    |> Maybe.withDefault "no-op i guess?"
                        in
                        ( nodes
                            |> Dict.remove indexToRemove
                        , edges
                            |> Dict.filter
                                (\id ( n1, n2 ) ->
                                    n1 /= indexToRemove && n2 /= indexToRemove
                                )
                        )
                    )
                    ( nodeDict, edgeDict )
    in
    Graph
        { nodes = newNodes
        , edges = newEdges
        }


empty : Graph
empty =
    Graph
        { nodes = Dict.empty
        , edges = Dict.empty
        }


isEmpty : Graph -> Bool
isEmpty (Graph model) =
    Dict.isEmpty model.nodes


getNode : String -> Graph -> Maybe Node
getNode id (Graph model) =
    Dict.get id model.nodes


getNodeUnsafe : String -> Graph -> Node
getNodeUnsafe id graph =
    case getNode id graph of
        Just pos ->
            pos

        Nothing ->
            Debug.todo ("can't find node id " ++ id)


getTouching : Float -> Pos -> Graph -> Maybe ( Node.Id, Node )
getTouching aspectRatio mousePos (Graph model) =
    model.nodes
        |> Dict.toList
        |> List.Extra.find
            (\( id, node ) -> isTouching aspectRatio mousePos node.pos)


getEdges : Graph -> Dict Edge.Id Edge
getEdges (Graph model) =
    model.edges


getNodes : Graph -> Dict Node.Id Node
getNodes (Graph model) =
    model.nodes


isTouching : Float -> Pos -> Pos -> Bool
isTouching aspectRatio mousePos nodePos =
    -- see https://math.stackexchange.com/questions/76457/check-if-a-point-is-within-an-ellipse
    let
        ( x, y ) =
            Pos.toTuple mousePos

        ( h, k ) =
            Pos.toTuple nodePos

        rx =
            Cfg.radius * (1 / aspectRatio)

        ry =
            Cfg.radius
    in
    ((((x - h) ^ 2) / (rx ^ 2)) + (((y - k) ^ 2) / ry ^ 2)) <= 1


isTouching_ : Float -> Pos -> Pos -> Bool
isTouching_ aspectRatio pos1 pos2 =
    let
        aSquared =
            (pos1.x - pos2.x) ^ 2

        bSquared =
            (pos1.y - pos2.y) ^ 2

        c =
            sqrt (aSquared + bSquared)
    in
    c <= Cfg.radius


setNodeDest : String -> Pos -> Graph -> Graph
setNodeDest nodeId dest (Graph model) =
    { model
        | nodes =
            model.nodes
                |> Dict.update nodeId
                    (Maybe.map (\node -> { node | dest = dest }))
    }
        |> Graph


applyAspectRatio : Float -> Graph -> Graph
applyAspectRatio ratio (Graph model) =
    { model
        | nodes =
            model.nodes
                |> Dict.map
                    (\id node ->
                        Node.applyAspectRatio ratio node
                    )
    }
        |> Graph


neighbors : Node.Id -> Graph -> Set Node.Id
neighbors nodeId (Graph model) =
    model.edges
        |> Dict.values
        |> List.filterMap
            (\( n1, n2 ) ->
                if n1 == nodeId then
                    Just n2

                else if n2 == nodeId then
                    Just n1

                else
                    Nothing
            )
        |> Set.fromList



-- juice stuff


animateNodes : Float -> Graph -> Graph
animateNodes timeElapsed (Graph model) =
    Graph
        { model
            | nodes =
                model.nodes
                    |> Dict.map (animateNode timeElapsed)
        }


animateNode : Float -> Node.Id -> Node -> Node
animateNode timeElapsed nodeId_ node =
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


moveNodesForLoadAnim : Float -> Graph -> Graph
moveNodesForLoadAnim timeElapsed (Graph model) =
    let
        numNodes =
            Dict.size model.nodes

        age =
            if timeElapsed < Cfg.wait then
                0

            else
                min Cfg.loadAnimDur (timeElapsed - Cfg.wait)

        ease =
            Ease.outElastic (age / Cfg.loadAnimDur)

        easeRot =
            Ease.outCubic (age / Cfg.loadAnimDur)

        newNodes =
            model.nodes
                |> Dict.map
                    (\id node ->
                        let
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
                    )
    in
    Graph { model | nodes = newNodes }


getNodeIdsInBox : Pos -> Pos -> Graph -> Set String
getNodeIdsInBox pos1 pos2 (Graph model) =
    model.nodes
        |> Dict.toList
        |> List.filter
            (\( id, node ) ->
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
                (boxX1 <= nodeX) && (nodeX <= boxX2) && (boxY1 <= nodeY) && (nodeY <= boxY2)
            )
        |> List.map Tuple.first
        |> Set.fromList


checkForIntersections : Graph -> Set Edge.Id
checkForIntersections ((Graph model) as graph) =
    -- O(n^2)
    -- See https://gist.github.com/Joncom/e8e8d18ebe7fe55c3894
    model.edges
        |> Dict.toList
        |> List.map
            (\( edgeId1, ( nodeIdA, nodeIdB ) ) ->
                let
                    ( ( x1, y1 ), ( x2, y2 ) ) =
                        ( getNodeUnsafe nodeIdA graph |> .pos |> Pos.toTuple
                        , getNodeUnsafe nodeIdB graph |> .pos |> Pos.toTuple
                        )
                in
                model.edges
                    |> Dict.toList
                    |> List.map
                        (\( edgeId2, ( nodeIdC, nodeIdD ) ) ->
                            if
                                [ nodeIdA
                                , nodeIdB
                                , nodeIdC
                                , nodeIdD
                                ]
                                    |> Set.fromList
                                    |> Set.size
                                    |> (==) 4
                            then
                                let
                                    ( ( x3, y3 ), ( x4, y4 ) ) =
                                        ( getNodeUnsafe nodeIdC graph |> .pos |> Pos.toTuple
                                        , getNodeUnsafe nodeIdD graph |> .pos |> Pos.toTuple
                                        )

                                    s1_x =
                                        x2 - x1

                                    s1_y =
                                        y2 - y1

                                    s2_x =
                                        x4 - x3

                                    s2_y =
                                        y4 - y3

                                    thing =
                                        -s2_x * s1_y + s1_x * s2_y

                                    s =
                                        (-s1_y * (x1 - x3) + s1_x * (y1 - y3)) / thing

                                    t =
                                        (s2_x * (y1 - y3) - s2_y * (x1 - x3)) / thing
                                in
                                if s >= 0 && s <= 1 && t >= 0 && t <= 1 then
                                    [ edgeId1, edgeId2 ]

                                else
                                    []

                            else
                                []
                        )
                    |> List.concat
            )
        |> List.concat
        |> Set.fromList



{-
   Maybe.map2
       (\nodeA nodeB ->
           model.edges
               |> Dict.toList
               |> List.filterMap
                   (\edgeId2 ( nodeIdC, nodeIdD ) ->
                       Maybe.map2
                           (\nodeC nodeD ->
                               f
                           )
                           (getNode nodeIdC graph)
                           (getNode nodeIdD graph)
                   )
       )
       (getNode nodeId1 graph)
       (getNode nodeId2 graph)
-}
-- codec stuff


codec : Codec Graph
codec =
    Codec.object Model
        |> Codec.field "nodes" .nodes (Codec.dict Node.codec)
        |> Codec.field "edges" .edges (Codec.dict (Codec.tuple Codec.string Codec.string))
        |> Codec.buildObject
        |> Codec.map Graph (\(Graph m) -> m)
