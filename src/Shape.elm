module Shape exposing (Shape)

import Pos exposing (Pos)


type alias Shape =
    { pts : List Pos
    , color : String --maybe Color?
    , dimmerAnimationDurationMs : Int
    , shimmerAnimationDelayMs : Int
    }
