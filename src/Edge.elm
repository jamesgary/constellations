module Edge exposing (Edge, Id, codec)

import Codec exposing (Codec)
import Node exposing (Node)


type alias Id =
    String


type alias Edge =
    ( Node.Id, Node.Id )



-- codec stuff


codec : Codec Edge
codec =
    Codec.tuple
        Codec.string
        Codec.string
