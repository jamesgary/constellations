module Edge exposing (Edge, Id)

import Node exposing (Node)


type alias Id =
    Int


type alias Edge =
    { id : Id
    , pair : ( Node.Id, Node.Id )
    }
