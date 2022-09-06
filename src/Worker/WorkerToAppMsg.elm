module Worker.WorkerToAppMsg exposing (WorkerToAppMsg(..), codec)

import Codec exposing (Codec)
import Edge exposing (Edge)
import Graph exposing (Graph)
import Set exposing (Set)


type WorkerToAppMsg
    = GeneratedGraph { graph : Graph }
    | GotIntersections { edges : Set Edge.Id }


codec : Codec WorkerToAppMsg
codec =
    Codec.custom
        (\generatedGraph gotIntersections val ->
            case val of
                GeneratedGraph arg ->
                    generatedGraph arg

                GotIntersections arg ->
                    gotIntersections arg
        )
        |> Codec.variant1
            "GeneratedGraph"
            GeneratedGraph
            (Codec.object (\graph -> { graph = graph })
                |> Codec.field "graph" .graph Graph.codec
                |> Codec.buildObject
            )
        |> Codec.variant1
            "GotIntersections"
            GotIntersections
            (Codec.object (\edges -> { edges = edges })
                |> Codec.field "edges" .edges (Codec.set Codec.string)
                |> Codec.buildObject
            )
        |> Codec.buildCustom
