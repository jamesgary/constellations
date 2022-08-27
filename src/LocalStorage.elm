module LocalStorage exposing (LocalStorage, codec)

import Codec exposing (Codec)
import Edge exposing (Edge)
import Node exposing (Node)


type alias LocalStorage =
    { numLevelsCleared : Int
    , levels : List Level
    }


type alias Level =
    { nodes : List Node
    , edges : List Edge
    }


init : LocalStorage
init =
    { numLevelsCleared = 0
    , levels = []
    }



-- codec stuff


codec : Codec LocalStorage
codec =
    Codec.oneOf
        (Codec.object LocalStorage
            |> Codec.field "numLevelsCleared" .numLevelsCleared Codec.int
            |> Codec.field "levels" .levels (Codec.list levelCodec)
            |> Codec.buildObject
        )
        [ Codec.succeed init ]


levelCodec : Codec Level
levelCodec =
    Codec.object Level
        |> Codec.field "nodes" .nodes (Codec.list Node.codec)
        |> Codec.field "edges" .edges (Codec.list Edge.codec)
        |> Codec.buildObject
