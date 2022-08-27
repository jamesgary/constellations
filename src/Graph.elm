module Graph exposing (Graph, animateNodes, codec, getEdges, getNode, getNodeIdsInBox, getNodeUnsafe, getNodes, getTouching, init, moveNodesForLoadAnim, setNodeDest)

import Cfg
import Codec exposing (Codec)
import Dict exposing (Dict)
import Ease
import Edge exposing (Edge)
import List.Extra
import Node exposing (Node)
import Pos exposing (Pos)
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
init difficulty =
    -- TODO actually make nodes/edges
    Graph
        { nodes =
            [ Pos 200 200
            , Pos 800 200
            , Pos 200 800
            , Pos 800 800
            ]
                |> List.indexedMap
                    (\i pos ->
                        ( String.fromInt i
                        , Node.init pos
                        )
                    )
                |> Dict.fromList
        , edges =
            [ ( 0, 1 )
            , ( 0, 2 )
            , ( 0, 3 )
            , ( 1, 2 )
            , ( 1, 3 )
            , ( 2, 3 )
            ]
                |> List.map (Tuple.mapBoth String.fromInt String.fromInt)
                |> List.indexedMap (\i edge -> ( String.fromInt i, edge ))
                |> Dict.fromList
        }


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


getTouching : Pos -> Graph -> Maybe ( Node.Id, Node )
getTouching mousePos (Graph model) =
    model.nodes
        |> Dict.toList
        |> List.Extra.find
            (\( id, node ) -> isTouching mousePos node.pos)


getEdges : Graph -> Dict Edge.Id Edge
getEdges (Graph model) =
    model.edges


getNodes : Graph -> Dict Node.Id Node
getNodes (Graph model) =
    model.nodes


isTouching : Pos -> Pos -> Bool
isTouching pos1 pos2 =
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



--moveNodesForLoadAnim : Float -> Int -> Node.Id -> Node -> Node
--moveNodesForLoadAnim timeElapsed numNodes id node =


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



-- codec stuff


codec : Codec Graph
codec =
    Codec.object Model
        |> Codec.field "nodes" .nodes (Codec.dict Node.codec)
        |> Codec.field "edges" .edges (Codec.dict (Codec.tuple Codec.string Codec.string))
        |> Codec.buildObject
        |> Codec.map Graph (\(Graph m) -> m)
