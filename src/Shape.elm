module Shape exposing (Shape, applyAspectRatio)

import Pos exposing (Pos)


type alias Shape =
    { pts : List Pos
    , color : String --maybe Color?
    , dimmerAnimationDurationMs : Int
    , shimmerAnimationDelayMs : Int
    }


applyAspectRatio : Float -> Shape -> Shape
applyAspectRatio ratio shape =
    { shape
        | pts =
            shape.pts
                |> List.map (Pos.applyAspectRatio ratio)
    }
