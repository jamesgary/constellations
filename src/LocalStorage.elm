module LocalStorage exposing (LocalStorage, codec)

import Array exposing (Array)
import Codec exposing (Codec)
import Edge exposing (Edge)
import Graph exposing (Graph)
import Node exposing (Node)


type alias LocalStorage =
    { numLevelsCleared : Int
    , levels : Array Graph
    }


init : LocalStorage
init =
    { numLevelsCleared = 0
    , levels = Array.empty
    }



-- codec stuff


codec : Codec LocalStorage
codec =
    Codec.oneOf
        (Codec.object LocalStorage
            |> Codec.field "numLevelsCleared" .numLevelsCleared Codec.int
            |> Codec.field "levels" .levels (Codec.array Graph.codec)
            |> Codec.buildObject
        )
        [ Codec.succeed init ]
