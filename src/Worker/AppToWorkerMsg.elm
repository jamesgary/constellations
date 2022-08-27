module Worker.AppToWorkerMsg exposing (AppToWorkerMsg(..), codec)

import Codec exposing (Codec)
import Graph exposing (Graph)


type AppToWorkerMsg
    = GenerateGraph { difficulty : Int }
    | CheckForIntersections { graph : Graph }



-- codec stuff


codec : Codec AppToWorkerMsg
codec =
    Codec.custom
        (\generateGraph checkForIntersections val ->
            case val of
                GenerateGraph arg ->
                    generateGraph arg

                CheckForIntersections arg ->
                    checkForIntersections arg
        )
        |> Codec.variant1
            "GenerateGraph"
            GenerateGraph
            (Codec.object (\diff -> { difficulty = diff })
                |> Codec.field "difficulty" .difficulty Codec.int
                |> Codec.buildObject
            )
        |> Codec.variant1
            "CheckForIntersections"
            CheckForIntersections
            (Codec.object (\graph -> { graph = graph })
                |> Codec.field "graph" .graph Graph.codec
                |> Codec.buildObject
            )
        |> Codec.buildCustom



{-
   |> Codec.variant1
       "CheckForIntersections"
       CheckForIntersections
       (Codec.object (\nodes edges -> { nodes = nodes, edges = edges })
           |> Codec.field "nodes" .nodes (Codec.list Node.codec)
           |> Codec.field "edges" .edges (Codec.list Edge.codec)
           |> Codec.buildObject
       )
-}
