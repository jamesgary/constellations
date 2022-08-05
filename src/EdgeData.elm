module EdgeData exposing (EdgeData)

import Edge exposing (Edge)


type alias EdgeData =
    -- edges, numNodes, difficulty
    ( List Edge, Int, Int )
