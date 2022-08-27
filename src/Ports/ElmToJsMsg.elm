module ElmToJsMsg exposing (ElmToJsMsg)


type AppToWorkerMsg
    = GenerateLevel { difficulty : Int }
    | CheckForIntersections { nodes : List Node, edges : List Edge }


codec : Codec AppToWorkerMsg
codec =
    Codec.custom
        (\generateLevel checkForIntersections val ->
            case val of
                GenerateLevel arg ->
                    generateLevel arg

                CheckForIntersections arg ->
                    checkForIntersections arg
        )
        |> Codec.variant1
            "GenerateLevel"
            GenerateLevel
            (Codec.object (\diff -> { difficulty = diff })
                |> Codec.field "difficulty" .difficulty Codec.int
                |> Codec.buildObject
            )
        |> Codec.variant1
            "CheckForIntersections"
            CheckForIntersections
            (Codec.object (\nodes edges -> { nodes = nodes, edges = edges })
                |> Codec.field "nodes" .nodes (Codec.list Node.codec)
                |> Codec.field "edges" .edges (Codec.list Edge.codec)
                |> Codec.buildObject
            )


type WorkerToAppMsg
    = GeneratedLevel { level : Level }
    | GotIntersections { edges : List Edge }
