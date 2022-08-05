module Node exposing (Id, Node)

import Pos exposing (Pos)
import Vel exposing (Vel)


type alias Id =
    Int


type alias Node =
    { id : Id
    , dest : Pos
    , pos : Pos
    , vel : Vel
    }
